module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Text()
import System.Exit

import KrakenExchange.API

-----------------------------------------------------------------------------

main :: IO ()
main = getConfig >>= either (const exitFailure) run 
  
run :: Config -> IO ()
run cfg = do
  r <-  runKrakenExchange cfg $ do
    -- io =<< time
    -- io =<< assets         (AssetOptions Currency [XXBT,XETH])
    -- io =<< assetPairs     (AssetPairOptions pairs)
    -- io =<< ticker         (TickerOptions pairs)
    -- io =<< ohlcs          (OHLCOptions xbtusd 60 Nothing)
    io =<< orderBook      (OrderBookOptions xbtusd (Just 5))
    -- io =<< trades         (TradesOptions xbtusd Nothing)
    -- io =<< spreads        (SpreadOptions xbtusd Nothing)
    -- io =<< balance
    -- io =<< tradeBalance   def
    -- io =<< openOrders     (OpenOrdersOptions True Nothing)
    -- io =<< closedOrders   def
    -- io =<< queryOrders    def
    -- io =<< tradesHistory  (TradesHistoryOptions (Just AllTradeTypes) True Nothing Nothing Nothing)
    -- io =<< queryTrades    (QueryTradesOptions ["123"] True)
    -- io =<< openPositions  (OpenPositionsOptions ["123"] False)
    -- io =<< ledgers        def
    -- io =<< queryLedgers   (QueryLedgersOptions ["123"])
    -- io =<< tradeVolume    (TradeVolumeOptions pairs)
  print r
 
 where

  io :: Show a => a -> KrakenExchangeT ()
  io t = liftIO $ print t >> putChar '\n' >> threadDelay 1000000
  
  xbtusd = AssetPair XXBT ZUSD
  xbteur = AssetPair XXBT ZEUR

  pairs  = [xbtusd,xbteur]

