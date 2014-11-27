module Main where

import Data.Word
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Maybe
import Data.List
import Data.Either

import Control.Applicative
import Control.Monad
import Control.Lens hiding ((.=))
import Control.Lens.TH

import System.Environment
import System.IO

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["user", "create"] -> do
      hostname <- getHostName
      BS.hPut stdout =<< httpPost [] (encode (object [ "devicetype" .= T.concat ["hhue#", T.pack (take 19 hostname)]]))
    ["user", "create", name] -> do
      hostname <- getHostName
      BS.hPut stdout =<< httpPost [] (encode (object [ "devicetype" .= T.concat ["hhue#", T.pack (take 19 hostname)]
                                                 , "username" .= name
                                                 ]))
    ["user", key, "delete", user] -> BS.hPut stdout =<< httpDelete [T.pack key, "config", "whitelist", T.pack user]
    ["config", key] -> BS.hPut stdout =<< httpGet [T.pack key, "config"]
    ["lights", key] -> BS.hPut stdout =<< httpGet [T.pack key, "lights"]
    ["light", key, number] -> BS.hPut stdout =<< httpGet [T.pack key, "lights", T.pack number]
    ["light", key, number, "name", name] ->
      BS.hPut stdout =<< httpPut [T.pack key, "lights", T.pack number] (encode (object ["name" .= T.pack name]))
    ["light", key, number, "pointsymbol", symbolNumber, symbol] ->
      BS.hPut stdout =<< httpPut [T.pack key, "lights", T.pack number, "pointsymbol"]
                             (encode (object [T.pack symbolNumber .= T.pack symbol]))
    "light":key:number:command ->
      case parseCommand lightCommand (map T.pack command) of
        Left err -> hPutStrLn stderr $ "couldn't parse light command: " ++ err
        Right cmd -> BS.hPut stdout =<< httpPut [T.pack key, "lights", T.pack number, "state"] (encode cmd)
    ["groups", key] -> BS.hPut stdout =<< httpGet [T.pack key, "groups"]
    "group":key:"create":name:lights -> BS.hPut stdout =<< httpPost [T.pack key, "groups"]
                                                                    (encode $ object [ "name" .= T.pack name
                                                                                     , "lights" .= map T.pack lights])
    "group":key:"update":number:name:lights ->
      BS.hPut stdout =<< httpPut [T.pack key, "groups", T.pack number]
                                 (encode $ object [ "name" .= T.pack name
                                                  , "lights" .= map T.pack lights])
    ["group", key, "delete", number] -> BS.hPut stdout =<< httpDelete [T.pack key, "groups", T.pack number]
    "group":key:number:command ->
      case parseCommand lightCommand (map T.pack command) of
        Left err -> hPutStrLn stderr $ "couldn't parse light command: " ++ err
        Right cmd -> BS.hPut stdout =<< httpPut [T.pack key, "groups", T.pack number, "action"] (encode cmd)
    _ -> hPutStrLn stderr "unrecognized command"

httpReq :: [Text] -> Data.ByteString.ByteString -> RequestBody -> IO ByteString
httpReq url method body = do
  request <- parseUrl . T.unpack . T.concat . intersperse "/" $ "http://philips-hue/api" : url
  response <- withManager defaultManagerSettings $ \mgr -> httpLbs (request { method = method
                                                                            , requestBody = body
                                                                            })
                                                                   mgr
  pure . responseBody $ response

httpGet :: [Text] -> IO ByteString
httpGet url = httpReq url "GET" (RequestBodyBS "")

httpPut :: [Text] -> ByteString -> IO ByteString
httpPut url body = httpReq url "PUT" (RequestBodyLBS body)

httpPost :: [Text] -> ByteString -> IO ByteString
httpPost url body = httpReq url "POST" (RequestBodyLBS body)

httpDelete :: [Text] -> IO ByteString
httpDelete url = httpReq url "DELETE" (RequestBodyBS "")
