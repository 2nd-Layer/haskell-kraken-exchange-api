module KrakenExchange.Parse where
  
import           Control.Applicative ((<|>))
import           Data.Aeson (Value(Null),FromJSON
                            ,withText
                            ,withObject
                            ,parseJSON,(.:))
import           Data.Aeson.Types (Parser)
import           Data.Scientific (Scientific)
import qualified Data.Text           as T (unpack)
-----------------------------------------------------------------------------

parseMaybeJustNull :: FromJSON a => Maybe Value -> Parser (Maybe a)
parseMaybeJustNull (Just v) = fmap Just (parseJSON v)
parseMaybeJustNull _        = return Nothing

parseMaybeNull :: FromJSON a => Value -> Parser (Maybe a)
parseMaybeNull Null = return Nothing
parseMaybeNull v = parseJSON v

parseResult :: FromJSON a => Value -> Parser a
parseResult = withObject "result" $ \o -> do
  (e :: [String]) <- o .: "error"
  case e of
    [] -> o .: "result"
    _  -> (fail . concat . map show) e

parseScientific :: Value -> Parser Scientific
parseScientific v = 
  withText "Scientific" (return . read . T.unpack) v
  <|>
  parseJSON v
