{-# LANGUAGE OverloadedStrings #-}

module CSS where

import Prelude hiding (div, (**))

import Control.Monad (forM_)
import Data.Text (Text)

import Clay

-- All these font names should exist in Google Fonts

headerFont :: Text
headerFont = "Comfortaa"

contentFont :: Text
contentFont = "Open Sans"

codeFont :: Text
codeFont = "Roboto Mono"

siteStyle :: Css
siteStyle = body ? do
  div # "#thesite" ? do
    fontFamily [contentFont] [sansSerif]
    forM_ [h1, h2, h3, h4, h5, h6, ".header"] $ \sel -> sel ?
      fontFamily [headerFont] [sansSerif]
    forM_ [pre, code, "tt"] $ \sel -> sel ?
      fontFamily [codeFont] [monospace]
    h1 ? textAlign center
    (article ** h2) ? color darkviolet
    (article ** img) ? do
      display block
      marginLeft Clay.auto
      marginRight Clay.auto
      width $ pct 50
    footer ? textAlign center
