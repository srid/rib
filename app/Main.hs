{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODo: What if I make this literate haskell thus blog post?
module Main where

import Prelude hiding (init, last)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Lens (at, (?~))
import Control.Monad (forM_, forever, guard, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (_Object)
import qualified Data.ByteString.Char8 as BS8
import Data.List (isSuffixOf, partition)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (withArgs)

import Network.Wai.Application.Static (defaultFileServerSettings, ssLookupFile, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import Safe (initMay, lastMay)
import System.Console.CmdArgs (Data, Typeable, auto, cmdArgs, help, modes, (&=))
import System.FSNotify (watchTree, withManager)
import WaiAppStatic.Types (LookupResult (..), Pieces, StaticSettings, fromPiece, unsafeToPiece)

import Development.Shake (Action, Verbosity (Chatty), copyFileChanged, getDirectoryFiles, need, readFile',
                          shakeArgs, shakeOptions, shakeVerbosity, want, writeFile', (%>), (|%>), (~>))
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.FilePath (dropDirectory1, dropExtension, (-<.>), (</>))
import Slick (compileTemplate', convert, jsonCache', markdownToHTML, substitute)

-- | HTML & CSS imports
import Clay (Css, background, body, white, (?))
import Reflex.Dom.Core hiding (Link, Space, def)
import Text.Pandoc


data App
  = Watch
  | Serve { port :: Int, watch :: Bool }
  | Generate
  deriving (Data,Typeable,Show,Eq)

cli :: App
cli = modes
  [ Watch
      &= help "Watch for changes and generate"
  , Serve
      { port = 8080 &= help "Port to bind to"
      , watch = False &= help "Watch in addition to serving generated files"
      } &= help "Serve the generated site"
  , Generate
      &= help "Generate the site"
      &= auto  -- | Generate is the default command.
  ]

-- | WAI Settings suited for serving statically generated websites.
staticSiteServerSettings :: FilePath -> StaticSettings
staticSiteServerSettings root = settings { ssLookupFile = lookupFileForgivingHtmlExt }
  where
    settings = defaultFileServerSettings root

    -- | Like upstream's `ssLookupFile` but ignores the ".html" suffix in the
    -- URL when looking up the corresponding file in the filesystem.
    --
    -- This allows "clean urls" so to speak.
    lookupFileForgivingHtmlExt :: Pieces -> IO LookupResult
    lookupFileForgivingHtmlExt pieces = ssLookupFile settings pieces >>= \case
      LRNotFound -> ssLookupFile settings (addHtmlExt pieces)
      x -> pure x

    -- | Add the ".html" suffix to the URL unless it already exists
    addHtmlExt :: Pieces -> Pieces
    addHtmlExt xs = fromMaybe xs $ do
      init <- fmap fromPiece <$> initMay xs
      last <- fromPiece <$> lastMay xs
      guard $ not $ ".html" `isSuffixOf` T.unpack last
      pure $ fmap unsafeToPiece $ init <> [last <> ".html"]


main :: IO ()
main = runApp =<< cmdArgs cli

runApp :: App -> IO ()
runApp = \case
  Watch -> withManager $ \mgr -> do
    -- Generate once
    runApp Generate
    -- And then every time a file changes under the ./site directory.
    void $ watchTree mgr "site" (const True) $ const $ runApp Generate
    -- Wait forever, effectively.
    (_,bs) <- renderStatic $ do
      el "p" $ text "hello world"
    BS8.putStrLn bs
    forever $ threadDelay maxBound

  Serve p w -> concurrently_
    (when w $ runApp Watch)
    -- FIXME: don't use "dist" as it is shared by cabal.
    (putStrLn ("Serving at " <> show p) >> Warp.run p (staticApp $ staticSiteServerSettings "dist"))

  Generate -> withArgs [] $ shakeArgs shakeOptions {shakeVerbosity = Chatty} $ do
    -- ^ The withArgs above is to ensure that our own app arguments is not
    -- confusing Shake.

    -- TODO: Understand how this works. The caching from Slick.
    getPostCached <- jsonCache' getPost

    want ["site"]

    -- Require all the things we need to build the whole site
    "site" ~>
      need ["static", "posts", "dist/index.html"]

    let staticFilePatterns = ["css//*", "js//*", "images//*"]
        -- ^ Which files are considered to be static files.
        postFilePatterns = ["*.md"]
        -- ^ Which files are considered to be post files

    -- Require all static assets
    "static" ~> do
      need . fmap ("dist" </>) =<< getDirectoryFiles "site" staticFilePatterns

    -- Rule for handling static assets, just copy them from source to dest
    ("dist" </>) <$> staticFilePatterns |%> \out ->
      copyFileChanged (destToSrc out) out

    -- Find and require every post to be built
    "posts" ~> do
      need . fmap (("dist" </>) . (-<.> "html")) =<< getDirectoryFiles "site" postFilePatterns

    -- build the main table of contents
    "dist/index.html" %> \out -> do
      posts <- traverse (getPostCached . PostFilePath . ("site" </>)) =<< getDirectoryFiles "site" postFilePatterns
      let indexInfo = uncurry IndexInfo $ partition ((== Just Programming) . category) posts
      -- NOTE: compileTemplate' does a `need` on the template file.
      writeFile' out =<< renderTemplate'' "site/templates/index.html" indexInfo

    -- rule for actually building posts
    "dist/*.html" %> \out -> do
      post <- getPost$ PostFilePath $ destToSrc out -<.> "md"
      html <- liftIO $ renderHTML $ pageHTML $ Page_Post post
      writeFile' out $ BS8.unpack html

  where
    -- | Read and parse a Markdown post
    getPost :: PostFilePath -> Action Post
    getPost (PostFilePath postPath) = do
      -- | Given a post source-file's file path as a cache key, load the Post object
      -- for it. This is used with 'jsonCache' to provide post caching.
      let srcPath = destToSrc postPath -<.> "md"
      m <- T.pack <$> readFile' srcPath
      postData <- markdownToHTML m
      let pm = either (error . show) id $ runPure $ readMarkdown markdownOptions m
      let postURL = T.pack $ srcToURL postPath
          withURL = _Object . at "url" ?~ Aeson.String postURL
          withMdContent = _Object . at "pandocDoc" ?~ Aeson.toJSON pm
          withSrc = _Object . at "srcPath" ?~ Aeson.String (T.pack srcPath)
      convert $ withSrc $ withURL $ withMdContent postData

    -- | Render a mustache template with the given object
    -- TODO: Use reflex static renderer instead of mustache's compileTemplate'
    renderTemplate'' t o = do
      template <- compileTemplate' t
      pure $ T.unpack $ substitute template $ Aeson.toJSON o

-- | Represents a HTML page that will be generated
data Page
  = Page_Index [Post]
  | Page_Post Post
  deriving (Generic, Show, FromJSON, ToJSON)

-- TODO: Tell Shake to regenerate when this function changes. How??
pageHTML :: DomBuilder t m => Page -> m ()
pageHTML page = do
  el "head" $ do
    el "title" $ text "TODO"
  el "body" $ do
    el "h1" $ text "This is page title"
    case page of
      Page_Index posts -> do
        el "h2" $ text "Index of posts"
        el "tt" $ text $ T.pack $ show posts
      Page_Post post -> do
        el "h2" $ text "Single post"
        -- TODO: Render Pandoc document straight to reflex
        -- https://hackage.haskell.org/package/pandoc-types-1.19/docs/Text-Pandoc-Definition.html#t:Block
        -- Is the pandoc `Walk` class of any use here?
        pandocHTML $ pandocDoc post

pandocHTML :: DomBuilder t m => Pandoc -> m ()
pandocHTML (Pandoc _meta blocks) = renderBlocks blocks
  where
    renderBlocks = mapM_ renderBlock
    renderBlock = \case
      Plain inlines -> renderInlines inlines
      Para xs -> el "p" $ renderInlines xs
      LineBlock xss -> forM_ xss $ \xs -> do
        renderInlines xs
        text "\n"
      CodeBlock attr x -> elPandocAttr "code" attr $ el "pre" $ text $ T.pack x
      v@(RawBlock _ _) -> notImplemented v
      BlockQuote xs -> el "blockquote" $ renderBlocks xs
      OrderedList lattr xss -> el "ol" $ do
        notImplemented lattr
        forM_ xss $ \xs -> el "li" $ renderBlocks xs
      BulletList xss -> el "ul" $ forM_ xss $ \xs -> el "li" $ renderBlocks xs
      DefinitionList defs -> el "dl" $ forM_ defs $ \(term, descList) -> do
        el "dt" $ renderInlines term
        forM_ descList $ \desc ->
          el "dd" $ renderBlocks desc
      Header level attr xs -> elPandocAttr (headerElement level) attr $ do
        renderInlines xs
      HorizontalRule -> el "hr" blank
      v@(Table _ _ _ _ _) -> notImplemented v
      Div attr xs -> elPandocAttr "div" attr $
        renderBlocks xs
      Null -> blank
    renderAttr (identifier, classes, attrs) =
       ( "id" =: T.pack identifier
      <> "class" =: T.pack (unwords classes)
       ) <> (Map.fromList $ fmap (\(x,y) -> (T.pack x, T.pack y)) attrs)
    elPandocAttr name = elAttr name . renderAttr
    headerElement level = case level of
      1 -> "h1"
      2 -> "h2"
      3 -> "h3"
      4 -> "h4"
      5 -> "h5"
      6 -> "h6"
      _ -> error "bad header level"
    renderInlines = mapM_ renderInline
    renderInline = \case
      Str x -> text $ T.pack x
      Emph xs -> el "em" $ renderInlines xs
      Strong xs -> el "strong" $ renderInlines xs
      Strikeout xs -> el "strike" $ renderInlines xs
      Superscript xs -> el "sup" $ renderInlines xs
      Subscript xs -> el "sub" $ renderInlines xs
      SmallCaps xs -> el "small" $ renderInlines xs
      v@(Quoted _qt _xs) -> notImplemented v
      v@(Cite _ _) -> notImplemented v
      Code attr x -> elPandocAttr "code" attr $
        text $ T.pack x
      Space -> text " " -- TODO: Reevaluate this.
      SoftBreak -> text " "
      LineBreak -> notImplemented LineBreak
      v@(Math _ _) -> notImplemented v
      v@(RawInline _ _) -> notImplemented v
      Link attr xs (lUrl, lTitle) -> do
        let attr' = renderAttr attr <> ("href" =: T.pack lUrl <> "title" =: T.pack lTitle)
        elAttr "a" attr' $ renderInlines xs
      Image attr xs (iUrl, iTitle) -> do
        let attr' = renderAttr attr <> ("src" =: T.pack iUrl <> "title" =: T.pack iTitle)
        elAttr "img" attr' $ renderInlines xs
      Note xs -> el "aside" $ renderBlocks xs
      Span attr xs -> elPandocAttr "span" attr $
        renderInlines xs
    notImplemented :: (DomBuilder t m, Show a) => a -> m ()
    notImplemented x = do
      el "strong" $ text "NOTIMPL"
      el "tt" $ text $ T.pack $ show x

renderHTML :: StaticWidget x a -> IO BS8.ByteString
renderHTML = fmap snd . renderStatic

siteStyle :: Css
siteStyle = body ? do
  background white

-- | Reasonable options for reading a markdown file
markdownOptions :: ReaderOptions
markdownOptions = def { readerExtensions = exts }
 where
  exts = mconcat
    [ extensionsFromList
      [ Ext_yaml_metadata_block
      , Ext_fenced_code_attributes
      , Ext_auto_identifiers
      ]
    , githubMarkdownExtensions
    ]

-- | Represents the template dependencies of the index page
-- TODO: Represent category of posts generically. dependent-map?
data IndexInfo = IndexInfo
  { programming_posts :: [Post]
  , other_posts :: [Post]
  } deriving (Generic, Show)

instance FromJSON IndexInfo
instance ToJSON IndexInfo

data PostCategory
  = Programming
  | Other
  deriving (Generic, Show, Eq, Ord)

instance FromJSON PostCategory
instance ToJSON PostCategory

-- | A JSON serializable representation of a post's metadata
-- TODO: Use Text instead of String
data Post = Post
  { title :: String
  , description :: String
  , category :: Maybe PostCategory
  , content :: String
  , pandocDoc :: Pandoc
  , url :: String
  } deriving (Generic, Eq, Ord, Show)

instance FromJSON Post
instance ToJSON Post


-- A simple wrapper data-type which implements 'ShakeValue';
-- Used as a Shake Cache key to build a cache of post objects.
newtype PostFilePath = PostFilePath FilePath
  deriving (Show, Eq, Hashable, Binary, NFData, Generic)

-- | convert 'build' filepaths into source file filepaths
destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

-- | convert a source file path into a URL
srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . dropExtension
