module KrakenExchange.Rest where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT,runReaderT,ask)
import           Crypto.Hash (SHA256,SHA512,HMAC,Digest
                             ,hmac,hash)
import           Data.Aeson.Types (Value)
import           Data.Byteable (toBytes)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.ByteString.Lazy as BL (toStrict)
import           Data.Monoid ()
import           Data.Proxy (Proxy(Proxy))
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time (getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           GHC.TypeLits (Symbol)
import           Servant.API ((:<|>)(..),(:>)
                             ,Post,ReqBody,JSON,Header,FormUrlEncoded
                             ,safeLink,mimeRender)
import           Servant.Client (ClientM,runClientM,client,ServantError,mkClientEnv
                                ,BaseUrl(BaseUrl),Scheme(Https))

import           KrakenExchange.Types (TradeVolumeOptions
                              ,QueryLedgers,QueryLedgersOptions
                              ,Ledgers,LedgersOptions
                              ,OpenPositionsOptions
                              ,QueryTrades,QueryTradesOptions
                              
                              ,QueryOrders,QueryOrdersOptions
                              ,ClosedOrders,ClosedOrdersOptions
                              ,OpenOrders,OpenOrdersOptions
                              ,TradeBalanceOptions,Balance
                              ,Spreads,SpreadOptions
                              ,Trades,TradesOptions

                              ,TradeBalance
                              ,TradesHistory,TradesHistoryOptions
                              ,OrderBook,OrderBookOptions
                              ,OHLCs,OHLCOptions
                              ,Ticker,TickerOptions

                              ,AssetPairOptions
                              ,AssetOptions
                              ,Assets
                              ,AssetPairs
                              

                              ,Config(Config),Time
                              ,configAPIKey

                              ,PrivateRequest(PrivateRequest)
                              ,configPassword,configPrivateKey
                              
                              ,Host,Port

                              )

import           Web.FormUrlEncoded (ToForm)
import           Network.HTTP.Client.TLS (newTlsManager)
-----------------------------------------------------------------------------

restHost :: Host
restHost = "api.kraken.com"

restPort :: Port
restPort = 443

-----------------------------------------------------------------------------

-- type ServantT = ExceptT ServantError IO
type KrakenExchangeT  = ReaderT Config ClientM

runKrakenExchange :: Config -> KrakenExchangeT a -> IO (Either ServantError a)
runKrakenExchange cfg ka = do
    mgr <- newTlsManager
    let env = mkClientEnv mgr baseUrl
    flip runClientM env $ runReaderT ka cfg 
  where baseUrl = BaseUrl Https restHost restPort ""


-----------------------------------------------------------------------------

type KrakenExchangeAPI     = TimeService
                        :<|> AssetService
                        :<|> AssetPairService
                        :<|> TickerService
                        :<|> OHLCService
                        :<|> OrderBookService
                        :<|> TradesService
                        :<|> SpreadService
                        :<|> BalanceService
                        :<|> TradeBalanceService
                        :<|> OpenOrdersService
                        :<|> ClosedOrdersService
                        :<|> QueryOrdersService
                        :<|> TradesHistoryService
                        :<|> QueryTradesService
                        :<|> OpenPositionsService
                        :<|> LedgersService
                        :<|> QueryLedgersService
                        :<|> TradeVolumeService

type TimeService           = PublicService  "Time"          ()                   Time
type AssetService          = PublicService  "Assets"        AssetOptions         Assets
type AssetPairService      = PublicService  "AssetPairs"    AssetPairOptions     AssetPairs
type TickerService         = PublicService  "Ticker"        TickerOptions        Ticker
type OHLCService           = PublicService  "OHLC"          OHLCOptions          OHLCs
type OrderBookService      = PublicService  "Depth"         OrderBookOptions     OrderBook
type TradesService         = PublicService  "Trades"        TradesOptions        Trades
type SpreadService         = PublicService  "Spread"        SpreadOptions        Spreads
type BalanceService        = PrivateService "Balance"       ()                   Balance
type TradeBalanceService   = PrivateService "TradeBalance"  TradeBalanceOptions  TradeBalance
type OpenOrdersService     = PrivateService "OpenOrders"    OpenOrdersOptions    OpenOrders
type ClosedOrdersService   = PrivateService "ClosedOrders"  ClosedOrdersOptions  ClosedOrders
type QueryOrdersService    = PrivateService "QueryOrders"   QueryOrdersOptions   QueryOrders
type TradesHistoryService  = PrivateService "TradesHistory" TradesHistoryOptions TradesHistory
type QueryTradesService    = PrivateService "QueryTrades"   QueryTradesOptions   QueryTrades
type OpenPositionsService  = PrivateService "OpenPositions" OpenPositionsOptions Value
type LedgersService        = PrivateService "Ledgers"       LedgersOptions       Ledgers
type QueryLedgersService   = PrivateService "QueryLedgers"  QueryLedgersOptions  QueryLedgers
type TradeVolumeService    = PrivateService "TradeVolume"   TradeVolumeOptions   Value

-----------------------------------------------------------------------------

type APIVersion            = "0"
type Public                = "public"
type Private               = "private"

-----------------------------------------------------------------------------

type PublicService 
     (a :: Symbol) b c     = APIVersion
                             :> Public
                             :> a
                             :> ReqBody '[FormUrlEncoded] b
                             :> Post '[JSON] c
type PrivateService
     (a :: Symbol) b c     = APIVersion
                             :> Private
                             :> a
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivateRequest b)
                             :> Post '[JSON] c

-----------------------------------------------------------------------------

api :: Proxy KrakenExchangeAPI
api = Proxy

-----------------------------------------------------------------------------

time_          :: () -> ClientM Time
assets_        :: AssetOptions -> ClientM Assets
assetPairs_    :: AssetPairOptions -> ClientM AssetPairs
ticker_        :: TickerOptions -> ClientM Ticker
ohlcs_         :: OHLCOptions -> ClientM OHLCs
orderBook_     :: OrderBookOptions -> ClientM OrderBook
trades_        :: TradesOptions -> ClientM Trades
spreads_       :: SpreadOptions -> ClientM Spreads
balance_       :: Maybe Text -> Maybe Text -> PrivateRequest () -> ClientM Balance
tradeBalance_  :: Maybe Text -> Maybe Text -> PrivateRequest TradeBalanceOptions -> ClientM TradeBalance
openOrders_    :: Maybe Text -> Maybe Text -> PrivateRequest OpenOrdersOptions -> ClientM OpenOrders
closedOrders_  :: Maybe Text -> Maybe Text -> PrivateRequest ClosedOrdersOptions -> ClientM ClosedOrders
queryOrders_   :: Maybe Text -> Maybe Text -> PrivateRequest QueryOrdersOptions -> ClientM QueryOrders
tradesHistory_ :: Maybe Text -> Maybe Text -> PrivateRequest TradesHistoryOptions -> ClientM TradesHistory
queryTrades_   :: Maybe Text -> Maybe Text -> PrivateRequest QueryTradesOptions -> ClientM QueryTrades
openPositions_ :: Maybe Text -> Maybe Text -> PrivateRequest OpenPositionsOptions -> ClientM Value
ledgers_       :: Maybe Text -> Maybe Text -> PrivateRequest LedgersOptions -> ClientM Ledgers
queryLedgers_  :: Maybe Text -> Maybe Text -> PrivateRequest QueryLedgersOptions -> ClientM QueryLedgers
tradeVolume_   :: Maybe Text -> Maybe Text -> PrivateRequest TradeVolumeOptions -> ClientM Value

time_
  :<|> assets_
  :<|> assetPairs_
  :<|> ticker_
  :<|> ohlcs_
  :<|> orderBook_
  :<|> trades_
  :<|> spreads_
  :<|> balance_ 
  :<|> tradeBalance_
  :<|> openOrders_
  :<|> closedOrders_
  :<|> queryOrders_
  :<|> tradesHistory_
  :<|> queryTrades_
  :<|> openPositions_
  :<|> ledgers_
  :<|> queryLedgers_
  :<|> tradeVolume_  = client api -- (BaseUrl Https restHost restPort)

-----------------------------------------------------------------------------

privateRequest :: ToForm a =>
                  String ->
                  a ->
                  (Maybe Text -> Maybe Text -> PrivateRequest a -> ClientM b) ->
                  KrakenExchangeT b
privateRequest url d f = do
  Config{..}       <- ask
  utcTime          <- liftIO getCurrentTime
  let apiKey       =  decodeUtf8 configAPIKey
      uri          =  BC.pack $ "/" <> url
      nonce        =  fromEnum . utcTimeToPOSIXSeconds $ utcTime
      privReq      =  PrivateRequest nonce configPassword d
      postData     =  BL.toStrict $ mimeRender (Proxy :: Proxy FormUrlEncoded) privReq
      nonceBytes   =  BC.pack . show $ nonce
      hashPostData =  toBytes (hash (nonceBytes <> postData) :: Digest SHA256)
      msg          =  uri <> hashPostData
      hmacMsg      =  hmac configPrivateKey msg :: HMAC SHA512
      apiSign      =  decodeUtf8 . B64.encode . toBytes $ hmacMsg
  lift $ f (Just apiKey) (Just apiSign) privReq

-----------------------------------------------------------------------------

time :: KrakenExchangeT Time
time = lift $ time_ ()

assets :: AssetOptions -> KrakenExchangeT Assets
assets = lift . assets_

assetPairs :: AssetPairOptions -> KrakenExchangeT AssetPairs
assetPairs = lift . assetPairs_

ticker :: TickerOptions -> KrakenExchangeT Ticker
ticker = lift . ticker_

ohlcs :: OHLCOptions -> KrakenExchangeT OHLCs
ohlcs = lift . ohlcs_

orderBook :: OrderBookOptions -> KrakenExchangeT OrderBook
orderBook = lift . orderBook_

trades :: TradesOptions -> KrakenExchangeT Trades
trades = lift . trades_

spreads :: SpreadOptions -> KrakenExchangeT Spreads
spreads = lift . spreads_

balance :: KrakenExchangeT Balance
balance = privateRequest 
  (show . safeLink api $ (Proxy :: Proxy BalanceService))
  ()
  balance_

tradeBalance :: TradeBalanceOptions -> KrakenExchangeT TradeBalance
tradeBalance opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy TradeBalanceService))
  opts
  tradeBalance_

openOrders :: OpenOrdersOptions -> KrakenExchangeT OpenOrders
openOrders opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy OpenOrdersService))
  opts
  openOrders_

closedOrders :: ClosedOrdersOptions -> KrakenExchangeT ClosedOrders
closedOrders opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy ClosedOrdersService))
  opts
  closedOrders_

queryOrders :: QueryOrdersOptions -> KrakenExchangeT QueryOrders
queryOrders opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy QueryOrdersService))
  opts
  queryOrders_

tradesHistory :: TradesHistoryOptions -> KrakenExchangeT TradesHistory
tradesHistory opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy TradesHistoryService))
  opts
  tradesHistory_

queryTrades :: QueryTradesOptions -> KrakenExchangeT QueryTrades
queryTrades opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy QueryTradesService))
  opts
  queryTrades_

openPositions :: OpenPositionsOptions -> KrakenExchangeT Value
openPositions opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy OpenPositionsService))
  opts
  openPositions_

ledgers :: LedgersOptions -> KrakenExchangeT Ledgers
ledgers opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy LedgersService))
  opts
  ledgers_

queryLedgers :: QueryLedgersOptions -> KrakenExchangeT QueryLedgers
queryLedgers opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy QueryLedgersService))
  opts
  queryLedgers_

tradeVolume :: TradeVolumeOptions -> KrakenExchangeT Value
tradeVolume opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy TradeVolumeService))
  opts
  tradeVolume_
