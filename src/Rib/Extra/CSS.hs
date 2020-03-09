{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Some commonly useful CSS styles
module Rib.Extra.CSS where

import Clay
import qualified Data.Text as T
import Lucid
import Relude

-- | Stock CSS for the <kbd> element
--
-- Based on the MDN demo at,
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
mozillaKbdStyle :: Css
mozillaKbdStyle = do
  backgroundColor $ rgb 238 238 238
  color $ rgb 51 51 51
  sym borderRadius (px 3)
  border solid (px 1) $ rgb 180 180 180
  padding (px 2) (px 4) (px 2) (px 4)
  boxShadow $
    (bsColor (rgba 0 0 0 0.2) $ shadowWithBlur (px 0) (px 1) (px 1))
      :| [(bsColor (rgba 255 255 255 0.7) $ bsInset $ shadowWithSpread (px 0) (px 2) (px 0) (px 0))]
  fontSize $ em 0.85
  fontWeight $ weight 700
  lineHeight $ px 1
  whiteSpace nowrap

-- | Include the specified Google Fonts
googleFonts :: Monad m => [Text] -> HtmlT m ()
googleFonts fs =
  let fsEncoded = T.intercalate "|" $ T.replace " " "+" <$> fs
      fsUrl = "https://fonts.googleapis.com/css?family=" <> fsEncoded <> "&display=swap"
   in stylesheet fsUrl

-- | Include the specified stylesheet URL
stylesheet :: Monad m => Text -> HtmlT m ()
stylesheet x = link_ [rel_ "stylesheet", href_ x]
