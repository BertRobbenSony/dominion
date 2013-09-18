{-# LANGUAGE OverloadedStrings #-}
module Handler.ErrorCode where

import Import
import Data.Aeson as Aeson
import Data.Text()
import Network.HTTP.Types

data ErrorCode = ErrorCode Status Int Text

returnError :: ErrorCode -> Handler RepJson
returnError (ErrorCode s c msg) = do
    repJson <- jsonToRepJson $ Aeson.object [ "errorCode" .= c, "message" .= msg ]
    sendResponseStatus s repJson

unknownGame :: ErrorCode
unknownGame = ErrorCode notFound404 1 "Unknown game"

unknownPlayer :: ErrorCode
unknownPlayer = ErrorCode notFound404 2 "Unknown player"

malformedBody :: ErrorCode
malformedBody = ErrorCode badRequest400 3 "Malformed body"

illegalMove :: Text -> ErrorCode
illegalMove txt = ErrorCode badRequest400 4 txt

liftErrorCode :: (a -> ErrorCode) -> Either a b -> Either ErrorCode b
liftErrorCode err (Left txt) = Left $ err txt
liftErrorCode _ (Right r) = Right r

