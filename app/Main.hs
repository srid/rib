{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding (div, init, last, (**))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad (forM_, forever, guard, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS8
import Data.List (isSuffixOf, partition)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import System.Environment (withArgs)

import Network.Wai.Application.Static (defaultFileServerSettings, ssLookupFile, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import Safe (initMay, lastMay)
import System.Console.CmdArgs (Data, Typeable, auto, cmdArgs, help, modes, (&=))
import System.FSNotify (watchTree, withManager)
import WaiAppStatic.Types (LookupResult (..), Pieces, StaticSettings, fromPiece, unsafeToPiece)

import Development.Shake (Action, Rebuild (..), Verbosity (Chatty), copyFileChanged, getDirectoryFiles, need,
                          readFile', shakeArgs, shakeOptions, shakeRebuild, shakeVerbosity, want, writeFile',
                          (%>), (|%>), (~>))
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.FilePath (dropDirectory1, dropExtension, (-<.>), (</>))
import Slick (jsonCache')

-- | HTML & CSS imports
import Clay (Css, article, block, body, center, code, color, darkviolet, display, div, fontFamily, footer, h1,
             h2, h3, h4, h5, h6, img, marginLeft, marginRight, monospace, pct, pre, render, sansSerif,
             textAlign, width, ( # ), (**), (?))
import qualified Clay
import Reflex.Dom.Core hiding (Link, Space, def, display)
import Text.Pandoc hiding (trace)
import Text.Pandoc.UTF8 (fromStringLazy)


data App
  = Watch
  | Serve { port :: Int, watch :: Bool }
  | Generate { force :: Bool }
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
      { force = False &= help "Force generation of all files"
      } &= help "Generate the site"
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

destDir :: FilePath
destDir = "generated"

contentDir :: FilePath
contentDir = "site"

main :: IO ()
main = runApp =<< cmdArgs cli

runApp :: App -> IO ()
runApp = \case
  Watch -> withManager $ \mgr -> do
    -- Begin with a *full* generation as the HTML layout may have been changed.
    runApp $ Generate True
    -- And then every time a file changes under the content directory.
    void $ watchTree mgr contentDir (const True) $ const $ runApp $ Generate False
    -- Wait forever, effectively.
    forever $ threadDelay maxBound

  Serve p w -> concurrently_
    (when w $ runApp Watch)
    (putStrLn ("Serving at " <> show p) >> Warp.run p (staticApp $ staticSiteServerSettings destDir))

  Generate forceGen -> withArgs [] $ do
    -- ^ The withArgs above is to ensure that our own app arguments is not
    -- confusing Shake.
    let opts = shakeOptions
          { shakeVerbosity = Chatty
          , shakeRebuild = bool [] ((RebuildNow,) <$> postFilePatterns) forceGen
          }
    shakeArgs opts $ do
      -- TODO: Write my own jsonCache and stop depending on `Slick`
      getPostCached <- jsonCache' getPost

      want ["site"]

      -- Require all the things we need to build the whole site
      "site" ~>
        need ["static", "posts", destDir </> "index.html"]

      -- Require all static assets
      "static" ~> do
        need . fmap (destDir </>) =<< getDirectoryFiles contentDir staticFilePatterns

      -- Rule for handling static assets, just copy them from source to dest
      (destDir </>) <$> staticFilePatterns |%> \out ->
        copyFileChanged (destToSrc out) out

      -- Find and require every post to be built
      "posts" ~> do
        files <- getDirectoryFiles contentDir postFilePatterns
        need $ (destDir </>) . (-<.> "html") <$> files

      -- build the main table of contents
      (destDir </> "index.html") %> \out -> do
        posts <- traverse (getPostCached . PostFilePath . (contentDir </>)) =<< getDirectoryFiles contentDir postFilePatterns
        html <- liftIO $ renderHTML $ pageHTML $ Page_Index posts
        writeFile' out $ BS8.unpack html

      -- rule for actually building posts
      (destDir </> "*.html") %> \out -> do
        post <- getPostCached $ PostFilePath $ destToSrc out -<.> "md"
        html <- liftIO $ renderHTML $ pageHTML $ Page_Post post
        writeFile' out $ BS8.unpack html

  where
    staticFilePatterns = ["images//*"]
    -- ^ Which files are considered to be static files.
    postFilePatterns = ["**/*.md"]
    -- ^ Which files are considered to be post files

    -- | Read and parse a Markdown post
    getPost :: PostFilePath -> Action Post
    getPost (PostFilePath postPath) = do
      let srcPath = destToSrc postPath -<.> "md"
      content <- T.pack <$> readFile' srcPath
      let doc = either (error . show) id $ runPure $ readMarkdown markdownOptions content
          postURL = T.pack $ srcToURL postPath
      pure $ Post doc postURL

-- | Represents a HTML page that will be generated
data Page
  = Page_Index [Post]
  | Page_Post Post
  deriving (Generic, Show, FromJSON, ToJSON)

-- | A JSON serializable representation of a post's metadata
data Post = Post
  { _post_doc :: Pandoc
  , _post_url :: Text
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

data PostCategory
  = Programming
  | Other
  deriving (Generic, Show, Eq, Ord, FromJSON, ToJSON)


-- A simple wrapper data-type which implements 'ShakeValue';
-- Used as a Shake Cache key to build a cache of post objects.
newtype PostFilePath = PostFilePath FilePath
  deriving (Show, Eq, Hashable, Binary, NFData, Generic)


-- | The entire HTML layout is here.
--
-- see `pandocHTML` for markdown HTML layout.
pageHTML :: DomBuilder t m => Page -> m ()
pageHTML page = do
  let pageTitle = case page of
        Page_Index _ -> text "Srid's notes"
        Page_Post post -> postTitleHTML post
  el "head" $ do
    elMeta "description" "Sridhar's notes"
    elMeta "author" "Sridhar Ratnakumar"
    elMeta "viewport" "width=device-width, initial-scale=1"
    el "title" pageTitle
    elAttr "style" ("type" =: "text/css") $ text $ TL.toStrict $ render siteStyle
    elAttr "link" ("rel" =: "stylesheet" <> "href" =: semUiCdn) blank
  el "body" $ do
    elAttr "div" ("class" =: "ui text container" <> "id" =: "thesite") $ do
      el "br" blank
      divClass "ui raised segment" $ do
        elAttr "a" ("class" =: "ui violet ribbon label" <> "href" =: "/") $ text "Srid's notes"

        elClass "h1" "ui huge header" pageTitle
        case page of
          Page_Index posts -> do
            let (progPosts, otherPosts) = partition ((== Just Programming) . getPostCategory) posts
            elClass "h2" "ui header" $ text "Haskell & Nix notes"
            postList progPosts
            elClass "h2" "ui header" $ text "Other notes"
            postList otherPosts
          Page_Post post ->
            elClass "article" "post" $
              -- TODO: code syntax highlighting
              pandocHTML $ _post_doc post

        elAttr "a" ("class" =: "ui green right ribbon label" <> "href" =: "https://www.srid.ca") $ text "Sridhar Ratnakumar"
    el "br" blank
    el "br" blank
    mapM_ elLinkGoogleFont ["Open+Sans","Comfortaa", "Roboto+Mono"]
  where
    postTitleHTML :: DomBuilder t m => Post -> m ()
    postTitleHTML post =
      let (Pandoc meta _) = _post_doc post
      in case Map.lookup "title" (unMeta meta) of
        Just (MetaInlines inlines) -> pandocHTML $ Pandoc meta [Plain inlines]
        _ -> blank
    postDescriptionHTML :: DomBuilder t m => Post -> m ()
    postDescriptionHTML post =
      let (Pandoc meta _) = _post_doc post
      in case Map.lookup "description" (unMeta meta) of
        Just (MetaInlines inlines) -> pandocHTML $ Pandoc meta [Plain inlines]
        _ -> blank
    getPostCategory :: Post -> Maybe PostCategory
    getPostCategory post =
      let (Pandoc meta _) = _post_doc post
      in case Map.lookup "category" (unMeta meta) of
        -- HACK: wraping in quotes to make valid json.
        Just (MetaInlines [Str v]) -> Aeson.decode $ fromStringLazy $ "\"" <> v <> "\""
        _ -> Nothing
    semUiCdn = "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
    elLinkGoogleFont name =
      elAttr "link" ("href" =: fontUrl <> "rel" =: "stylesheet" <> "type" =: "text/css") blank
      where
        fontUrl = "https://fonts.googleapis.com/css?family=" <> name
    elMeta k v = elAttr "meta" ("name" =: k <> "content" =: v) blank
    postList ps = divClass "ui relaxed divided list" $ forM_ ps $ \p -> do
      divClass "item" $ do
        elAttr "a" ("class" =: "header" <> "href" =: _post_url p) $ postTitleHTML p
        el "small" $ postDescriptionHTML p


-- | Convert a Reflex DOM widget into HTML
renderHTML :: StaticWidget x a -> IO BS8.ByteString
renderHTML = fmap snd . renderStatic

siteStyle :: Css
siteStyle = body ? do
  div # "#thesite" ? do
    fontFamily ["Open Sans"] [sansSerif]
    forM_ [h1, h2, h3, h4, h5, h6, ".header"] $ \header -> header ?
      fontFamily ["Comfortaa"] [sansSerif]
    forM_ [pre, code, "tt"] $ \s -> s ?
      fontFamily ["Roboto Mono"] [monospace]
    h1 ? textAlign center
    (article ** h2) ? color darkviolet
    (article ** img) ? do
      display block
      marginLeft Clay.auto
      marginRight Clay.auto
      width $ pct 50
    footer ? textAlign center


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

-- | convert 'build' filepaths into source file filepaths
destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

-- | convert a source file path into a URL
srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . dropExtension


-- Innards
--

-- | Convert Markdown to HTML
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
    elPandocAttr name = elAttr name . renderAttr
    renderAttr (identifier, classes, attrs) =
         "id" =: T.pack identifier
      <> "class" =: T.pack (unwords classes)
      <> Map.fromList ((\(x,y) -> (T.pack x, T.pack y)) <$> attrs)
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
