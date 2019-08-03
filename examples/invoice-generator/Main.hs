{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock

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

-- TODO: Use safe-money
type MoneyUSD = Int
type Hours = Int

data Invoice = Invoice
  { _invoice_author :: Author
  , _invoice_company :: Company
  , _invoice_date :: Day
  , _invoice_hourlyRate :: MoneyUSD
  , _invoice_items :: [(Day, Hours)]
  }
  deriving (Eq, Show, Ord)

theInvoice :: Day -> Invoice
theInvoice day = Invoice
  { _invoice_author = Author "Sridhar Ratnakumar" "555-111-9020" "2142 8e Avenue" "Quebec H7S J8A"
  , _invoice_company = Company "Some Company" "29 W 18th St, 513" "NY 10011"
  , _invoice_date = day
  , _invoice_hourlyRate = 100
  , _invoice_items = thisWeekItems 8
  }
  where
    thisWeekItems hoursPerDay =
      let
        (y, w, _) = toWeekDate day
        daysWorked = [1..5] -- Monday to Friday
      in flip fmap daysWorked $ \d ->
        (fromWeekDate y w d, hoursPerDay)

main :: IO ()
main = App.run buildAction

buildAction :: Action ()
buildAction = do
  day <- utctDay <$> liftIO getCurrentTime
  S.buildHtml "index.html" $ renderPage $
    theInvoice day

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
  body_ $ with div_ [class_ "ui container"] $
    with div_ [class_ "ui center aligned padded grid ", id_ "thesite"] $ do
      row_ $ with div_ [class_ "olive column"] $
        h1_ pageTitle
      with div_ [class_ "two column row"] $ do
        with div_ [class_ "left floated six wide left aligned column"] $
          companyCard $ _invoice_company invoice
        with div_ [class_ "right floated four wide right aligned column"] $
          userCard $ _invoice_author invoice
      row_ $
        with div_ [class_ "sixteen wide column"] $
          with table_ [class_ "ui celled table"] $ do
            thead_ $ tr_ $ do
              th_ "Pos"
              th_ "Date"
              th_ "Description"
              th_ "Prices in USD"
            tbody_ $ forM_ (zip [1..] $ _invoice_items invoice) $ \(pos :: Int, (day, hours)) -> tr_ $ do
              td_ $ toHtml $ show pos
              td_ $ toHtml $ showGregorian day
              td_ $ toHtml $ show hours <> " working hours"
              td_ $ toHtml $ show $ hours * _invoice_hourlyRate invoice
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
