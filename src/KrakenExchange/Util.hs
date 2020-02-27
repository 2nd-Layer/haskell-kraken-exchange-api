module KrakenExchange.Util where

import           Data.Maybe (fromMaybe)
import           System.Envy (decodeEnv)

import           KrakenExchange.Types (Config,EnvVars(EnvVars)
                              ,envvarsAPIKey,envvarsPrivateKeyB64,envvarsPassword
                              ,mkConfig)

-----------------------------------------------------------------------------

getConfig :: IO (Either String Config)
getConfig = decodeEnv >>= return . either Left config
 where
  config :: EnvVars -> Either String Config
  config EnvVars{..} = mkConfig
    (fromMaybe "" envvarsAPIKey)
    (fromMaybe "" envvarsPrivateKeyB64)
    envvarsPassword

