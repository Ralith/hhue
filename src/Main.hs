module Main where

import Data.Word
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
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
    [key, "lights"] -> BS.hPut stdout =<< get (T.pack key) ["lights"]
    [key, "groups"] -> BS.hPut stdout =<< get (T.pack key) ["groups"]
    [key, "light", number] -> BS.hPut stdout =<< get (T.pack key) ["lights", T.pack number]
    [key, "light", number, "name", name] ->
      BS.hPut stdout =<< put (T.pack key) ["lights", T.pack number] (encode (object ["name" .= T.pack name]))
    key:"light":number:command ->
      case parseCommand lightCommand (map T.pack command) of
        Left err -> hPutStrLn stderr $ "couldn't parse light command: " ++ err
        Right cmd -> BS.hPut stdout =<< put (T.pack key) ["lights", T.pack number, "state"] (encode cmd)
    key:"group":number:command ->
      case parseCommand lightCommand (map T.pack command) of
        Left err -> hPutStrLn stderr $ "couldn't parse light command: " ++ err
        Right cmd -> BS.hPut stdout =<< put (T.pack key) ["groups", T.pack number, "action"] (encode cmd)
    _ -> hPutStrLn stderr "unrecognized command"

get :: Text -> [Text] -> IO ByteString
get key url = do
  request <- parseUrl . T.unpack $ T.concat ["http://philips-hue/api/", key, "/", T.intercalate "/" url]
  response <- withManager defaultManagerSettings $ \mgr -> httpLbs request mgr
  pure . responseBody $ response

put :: Text -> [Text] -> ByteString -> IO ByteString
put key url body = do
  request <- parseUrl . T.unpack $ T.concat ["http://philips-hue/api/", key, "/", T.intercalate "/" url]
  response <- withManager defaultManagerSettings $ \mgr -> httpLbs (request { method = "PUT"
                                                                            , requestBody = RequestBodyLBS body
                                                                            })
                                                                   mgr
  pure . responseBody $ response
