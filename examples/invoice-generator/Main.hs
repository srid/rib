{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (div, (**))

import Data.Text (Text)
import Data.Time.Calendar

import Clay hiding (type_)
import Development.Shake
import Lucid

import qualified Rib.App as App
import qualified Rib.Shake as S

data BankAccount = BankAccount

data Author = Author
  { _author_name :: Text
  , _author_phone :: Text
  }
  deriving (Eq, Show, Ord)

data Invoice = Invoice
  { _invoice_author :: Author
  , _invoice_date :: Day
  }
  deriving (Eq, Show, Ord)

theInvoice :: Invoice
theInvoice = Invoice
  { _invoice_author = Author "Sridhar Ratnakumar" "555-111-9020"
  , _invoice_date = fromGregorian 2019 7 19
  }

main :: IO ()
main = App.run buildAction

-- TODO: Once finished, read theInvoice from invoice.json
buildAction :: Action ()
buildAction = S.buildHtml "index.html" $ renderPage theInvoice

renderPage :: Invoice -> Html ()
renderPage invoice = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ pageTitle
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    style_ [type_ "text/css"] $ Clay.render pageStyle
    link_ [ rel_ "stylesheet"
          , href_ "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
          ]
  body_ $
    with div_ [class_ "ui text container", id_ "thesite"] $ do
      -- Main content
      h1_ pageTitle
      with article_ [class_ "invoice"] $
        p_ $ toHtml $ show invoice
  where
    pageTitle = "Invoice No. ???"

    -- | CSS
    pageStyle :: Css
    pageStyle = div # "#thesite" ? do
      marginLeft $ pct 20
      marginTop $ em 4
