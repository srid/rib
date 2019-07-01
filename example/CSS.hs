{-# LANGUAGE OverloadedStrings #-}

module CSS where

import Prelude hiding (div, (**))

import Control.Monad
import Data.Text (Text)

import Clay

googleFonts :: [Text]
googleFonts = [headerFont, contentFont, codeFont]

headerFont :: Text
headerFont = "Comfortaa"

contentFont :: Text
contentFont = "Open Sans"

codeFont :: Text
codeFont = "Roboto Mono"

-- | Main style for the site
style :: Css
style = body ? do
  div # "#thesite" ? do
    marginTop $ em 1
    marginBottom $ em 2
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
