{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Exception.Safe (throwIO)
import Data.ByteString.Char8 qualified as Bytestring
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import GitHub
import Relude
import Shh qualified
import "base64" Data.ByteString.Base64

main = do
  token <- Bytestring.strip . toStrict <$> (Shh.exe "rbw" "get" "github.com" "-f" "kass" Shh.|> Shh.captureTrim)
  response <- github (OAuth token) (getNotificationsR FetchAll)
  r <- either throwIO (pure . Vector.head) response
  putTextLn $ url r

url :: Notification -> Text
url notif =
  "https://github.com/"
    <> Text.replace
      "pulls"
      "pull"
      (Text.drop 29 (getUrl $ fromMaybe (error "url missing") notif.notificationSubject.subjectURL))
    <> "?notification_referrer_id=NT_"
    <> encodeBase64 ("\147\NUL\206\NUL\EM2}\179\&" <> encodeUtf8 (show (untagId notif.notificationId) <> ":1651325"))
