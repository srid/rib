{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Parsing Markdown using the mmark parser.
module Rib.Parser.MMark
  ( -- * Parsing
    parse,
    parsePure,
    parseWith,
    parsePureWith,
    defaultExts,

    -- * Rendering
    render,

    -- * Extracting information
    getFirstImg,
    projectYaml,

    -- * Re-exports
    MMark,
  )
where

import Control.Foldl (Fold (..))
import Development.Shake (readFile')
import Development.Shake
import Lucid (Html)
import Path
import Relude
import Rib.Shake (ribInputDir)
import Text.MMark (MMark, projectYaml)
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension as Ext
import qualified Text.MMark.Extension.Common as Ext
import qualified Text.Megaparsec as M
import Text.URI (URI)

-- | Render a MMark document as HTML
render :: MMark -> Html ()
render = MMark.render

-- | Like `parsePure` but takes a custom list of MMark extensions
parsePureWith ::
  [MMark.Extension] ->
  -- | Filepath corresponding to the text to be parsed (used only in parse errors)
  FilePath ->
  -- | Text to be parsed
  Text ->
  Either Text MMark
parsePureWith exts k s = case MMark.parse k s of
  Left e -> Left $ toText $ M.errorBundlePretty e
  Right doc -> Right $ MMark.useExtensions exts $ useTocExt doc

-- | Pure version of `parse`
parsePure ::
  -- | Filepath corresponding to the text to be parsed (used only in parse errors)
  FilePath ->
  -- | Text to be parsed
  Text ->
  Either Text MMark
parsePure = parsePureWith defaultExts

-- | Parse Markdown using mmark
parse :: Path Rel File -> Action (Either Text MMark)
parse = parseWith defaultExts

-- | Like `parse` but takes a custom list of MMark extensions
parseWith :: [MMark.Extension] -> Path Rel File -> Action (Either Text MMark)
parseWith exts f = do
  inputDir <- ribInputDir
  s <- toText <$> readFile' (toFilePath $ inputDir </> f)
  pure $ parsePureWith exts (toFilePath f) s

-- | Get the first image in the document if one exists
getFirstImg :: MMark -> Maybe URI
getFirstImg = flip MMark.runScanner $ Fold f Nothing id
  where
    f acc blk = acc <|> listToMaybe (mapMaybe getImgUri (inlinesContainingImg blk))
    getImgUri = \case
      Ext.Image _ uri _ -> Just uri
      _ -> Nothing
    inlinesContainingImg :: Ext.Bni -> [Ext.Inline]
    inlinesContainingImg = \case
      Ext.Naked xs -> toList xs
      Ext.Paragraph xs -> toList xs
      _ -> []

defaultExts :: [MMark.Extension]
defaultExts =
  [ Ext.fontAwesome,
    Ext.footnotes,
    Ext.kbd,
    Ext.linkTarget,
    Ext.mathJax (Just '$'),
    Ext.punctuationPrettifier,
    -- For list of parsers supported, see:
    -- https://github.com/jgm/skylighting/tree/master/skylighting-core/xml
    Ext.skylighting
  ]

useTocExt :: MMark -> MMark
useTocExt doc = MMark.useExtension (Ext.toc "toc" toc) doc
  where
    toc = MMark.runScanner doc $ Ext.tocScanner (\x -> x > 1 && x < 5)
