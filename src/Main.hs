module Main where

import Data.Word
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Aeson.Lens
import Data.Maybe
import Data.List
import Data.Either

import Control.Applicative
import Control.Monad
import Control.Lens hiding ((.=))
import Control.Lens.TH

import System.Environment
import System.IO
import System.IO.Error
import System.Exit
import System.Directory
import System.FilePath

import Network.HTTP.Client
import Network.HostName

data LightCommand = LightCommand { _power :: Maybe Bool
                                 , _brightness :: Maybe Word8
                                 , _hue :: Maybe Word16
                                 , _saturation :: Maybe Word8
                                 , _ciexy :: Maybe (Float, Float)
                                 , _colorTemp :: Maybe Word16
                                 , _alert :: Maybe Text
                                 , _effect :: Maybe Text
                                 , _transitionTime :: Maybe Word16
                                 }
makeLenses ''LightCommand

instance ToJSON LightCommand where
  toJSON cmd = object . catMaybes . map ($ cmd) $
               [ f "on" power
               , f "bri" brightness
               , f "hue" hue
               , f "sat" saturation
               , f "xy" ciexy
               , f "ct" colorTemp
               , f "alert" alert
               , f "effect" effect
               , f "transitiontime" transitionTime
               ]
    where
      f :: ToJSON a => Text -> (Lens' LightCommand (Maybe a)) -> LightCommand -> Maybe Pair
      f name accessor cmd = case view accessor cmd of
                              Nothing -> Nothing
                              Just val -> Just (name .= val)

lightCommand :: LightCommand
lightCommand = LightCommand Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

nullaryCommands :: [(Text, LightCommand -> LightCommand)]
nullaryCommands = [ ("on", set power (Just True))
                  , ("off", set power (Just False))]

unaryCommands :: [(Text, Text -> Either String (LightCommand -> LightCommand))]
unaryCommands = [ ("bri", fmap (set brightness . Just . fst) . TR.decimal)
                , ("hue", fmap (set hue . Just . fst) . TR.decimal)
                , ("sat", fmap (set saturation . Just . fst) . TR.decimal)
                , ("ct", fmap (set colorTemp . Just . fst) . TR.decimal)
                , ("alert", fmap (set alert . Just) . limit ["none", "select", "lselect"])
                , ("effect", fmap (set effect . Just) . limit ["none", "colorloop"])
                , ("time", fmap (set transitionTime . Just . fst) . TR.decimal)
                ]
  where
    limit :: [Text] -> Text -> Either String Text
    limit options input
      | elem input options = Right input
      | otherwise = Left $ "illegal input \"" ++ T.unpack input ++ "\", expected one of: " ++ intercalate ", " (map T.unpack options)

parseCommand :: LightCommand -> [Text] -> Either String LightCommand
parseCommand c [] = Right c
parseCommand c ((flip lookup nullaryCommands -> Just f):xs) = parseCommand (f c) xs
parseCommand c ((flip lookup unaryCommands -> Just p):arg:xs) = do
  f <- p arg
  parseCommand (f c) xs
parseCommand _ _ = Left "unrecognized command"

usage :: Text
usage = "Usage: hhue <config | lights | groups | user <create [key] | delete <key>> | light <light id> [name <name> | pointsymbol <pointsymbol id> <symbol> | <COMMAND>*] | group <create <name> <light id>* | update <group id> <name> <light id>* | delete <group id> | <group id> <COMMAND>*>>\n\
        \COMMAND := on | off | bri <0-255> | hue <0-65535> | sat <0-255> | ct <153-500> | alert <none|select|lselect> | effect <none|colorloop> | transitiontime <0-65535>\n\
        \ct represents color temperature in mireds\n\
        \transitiontime is in units of 100ms\n\
        \Arbitrarily many commands may be specified simultaneously. When a command recurs, the rightmost instance dominates."

main :: IO ()
main = do
  args <- getArgs
  configDir <- (</> ".config/hhue") <$> getHomeDirectory
  createDirectoryIfMissing True configDir
  let keyFile = configDir </> "key"
  key <- catchIOError (decodeUtf8 <$> BS.readFile keyFile) $ \err ->
    case isDoesNotExistError err of
      False -> ioError err
      True -> do
        result <- createUser Nothing
        case result ^? nth 0 . key "error" . key "description" . _String of
          Just reason -> do
            hPutStr stderr "failed to create user: "
            BS.hPut stderr (encodeUtf8 reason)
            hPutStrLn stderr ""
            exitFailure
          Nothing ->
            case result ^? nth 0 . key "success" . key "username" . _String of
              Nothing -> hPutStrLn stderr "failed to parse user create result: " >> BSL.hPut stderr result >> exitFailure
              Just freshKey -> BS.writeFile keyFile (encodeUtf8 freshKey) >> pure freshKey
  case args of
    ["user", "create"] -> BSL.hPut stdout =<< createUser Nothing
    ["user", "create", name] -> BSL.hPut stdout =<< createUser (Just . T.pack $ name)
    ["user", "delete", user] -> BSL.hPut stdout =<< httpDelete [key, "config", "whitelist", T.pack user]
    ["config"] -> BSL.hPut stdout =<< httpGet [key, "config"]
    ["lights"] -> BSL.hPut stdout =<< httpGet [key, "lights"]
    ["light", number] -> BSL.hPut stdout =<< httpGet [key, "lights", T.pack number]
    ["light", number, "name", name] ->
      BSL.hPut stdout =<< httpPut [key, "lights", T.pack number] (encode (object ["name" .= T.pack name]))
    ["light", number, "pointsymbol", symbolNumber, symbol] ->
      BSL.hPut stdout =<< httpPut [key, "lights", T.pack number, "pointsymbol"]
                             (encode (object [T.pack symbolNumber .= T.pack symbol]))
    "light":number:command ->
      case parseCommand lightCommand (map T.pack command) of
        Left err -> hPutStrLn stderr $ "couldn't parse light command: " ++ err
        Right cmd -> BSL.hPut stdout =<< httpPut [key, "lights", T.pack number, "state"] (encode cmd)
    ["groups"] -> BSL.hPut stdout =<< httpGet [key, "groups"]
    "group":"create":name:lights -> BSL.hPut stdout =<< httpPost [key, "groups"]
                                                                    (encode $ object [ "name" .= T.pack name
                                                                                     , "lights" .= map T.pack lights])
    "group":"update":number:name:lights ->
      BSL.hPut stdout =<< httpPut [key, "groups", T.pack number]
                                 (encode $ object [ "name" .= T.pack name
                                                  , "lights" .= map T.pack lights])
    ["group", "delete", number] -> BSL.hPut stdout =<< httpDelete [key, "groups", T.pack number]
    "group":number:command ->
      case parseCommand lightCommand (map T.pack command) of
        Left err -> hPutStrLn stderr $ "couldn't parse light command: " ++ err
        Right cmd -> BSL.hPut stdout =<< httpPut [key, "groups", T.pack number, "action"] (encode cmd)
    _ -> BS.hPut stderr . encodeUtf8 $ T.concat ["unrecognized command\n", usage, "\n"]

httpReq :: [Text] -> ByteString -> RequestBody -> IO BSL.ByteString
httpReq url method body = do
  request <- parseUrl . T.unpack . T.concat . intersperse "/" $ "http://philips-hue/api" : url
  response <- withManager defaultManagerSettings $ \mgr -> httpLbs (request { method = method
                                                                            , requestBody = body
                                                                            })
                                                                   mgr
  pure . responseBody $ response

httpGet :: [Text] -> IO BSL.ByteString
httpGet url = httpReq url "GET" (RequestBodyBS "")

httpPut :: [Text] -> BSL.ByteString -> IO BSL.ByteString
httpPut url body = httpReq url "PUT" (RequestBodyLBS body)

httpPost :: [Text] -> BSL.ByteString -> IO BSL.ByteString
httpPost url body = httpReq url "POST" (RequestBodyLBS body)

httpDelete :: [Text] -> IO BSL.ByteString
httpDelete url = httpReq url "DELETE" (RequestBodyBS "")

createUser :: Maybe Text -> IO BSL.ByteString
createUser Nothing = do
  hostname <- getHostName
  httpPost [] (encode (object [ "devicetype" .= T.concat ["hhue#", T.pack (take 19 hostname)]]))
createUser (Just name) = do
  hostname <- getHostName
  httpPost [] (encode (object [ "devicetype" .= T.concat ["hhue#", T.pack (take 19 hostname)]
                             , "username" .= name
                             ]))
