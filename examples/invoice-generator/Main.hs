{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

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
  , _author_address1 :: Text
  , _author_address2 :: Text
  }
  deriving (Eq, Show, Ord)

data Company = Company
  { _company_name :: Text
  , _company_address1 :: Text
  , _company_address2 :: Text
  }
  deriving (Eq, Show, Ord)

data Invoice = Invoice
  { _invoice_author :: Author
  , _invoice_company :: Company
  , _invoice_date :: Day
  }
  deriving (Eq, Show, Ord)

theInvoice :: Invoice
theInvoice = Invoice
  { _invoice_author = Author "Sridhar Ratnakumar" "555-111-9020" "2142 4e Avenue" "Quebec H7S J8A"
  , _invoice_company = Company "Obsidian Systems" "19 W 21st St, 503" "NY 10011"
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
  body_ $ with div_ [class_ "ui text container"] $
    with div_ [class_ "ui center aligned padded grid ", id_ "thesite"] $ do
      row_ $ with div_ [class_ "olive column"] $
        h1_ pageTitle
      row_ $ do
        with div_ [class_ "left floated six wide left aligned column"] $
          companyCard $ _invoice_company invoice
        with div_ [class_ "right floated six wide right aligned column"] $
          userCard $ _invoice_author invoice
      row_ $
        with div_ [class_ "sixteen wide column"] $
          with table_ [class_ "ui celled table"] $ do
            thead_ $ tr_ $ do
              th_ "Pos"
              th_ "Description"
              th_ "Prices in INR"
            tbody_ $ tr_ $ do
              td_ "1"
              td_ "8 working hours on Jul 27"
              td_ "25000"
  where
    pageTitle = toHtml $ "Invoice #" <> show (_invoice_date invoice)

    companyCard :: Company -> Html ()
    companyCard Company {..} = with div_ [class_ "ui card"] $ do
      with div_ [class_ "image"] mempty
      with div_ [class_ "content"] $ do
        with a_ [class_ "header"] $ toHtml _company_name
        with div_ [class_ "description"] $ do
          p_ $ toHtml _company_address1
          p_ $ toHtml _company_address2

    userCard :: Author -> Html ()
    userCard Author {..} = with div_ [class_ "ui card"] $ do
      with div_ [class_ "image"] mempty
      with div_ [class_ "content"] $ do
        with a_ [class_ "header"] $ toHtml _author_name
        with div_ [class_ "description"] $ do
          p_ $ toHtml _author_address1
          p_ $ toHtml _author_address2

    row_ = with div_ [class_ "row"]
    -- col_ = with div_ [class_ "column"]

    -- | CSS
    pageStyle :: Css
    pageStyle = div # "#thesite" ?
      mempty
