module Mail.Hailgun.SendEmail
    ( sendEmail
    , HailgunSendResponse(..)
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Mail.Hailgun.Communication
import Mail.Hailgun.Errors
import Mail.Hailgun.Internal.Data
import Mail.Hailgun.MailgunApi
import Mail.Hailgun.PartUtil
import Network.HTTP.Client (httpLbs, newManager)
import qualified Network.HTTP.Client.MultipartFormData as NCM
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Email.Validate (EmailAddress, toByteString)

-- | Send an email using the Mailgun API's. This method is capable of sending a message over the
-- Mailgun service. All it needs is the appropriate context.
sendEmail
   :: HailgunContext -- ^ The Mailgun context to operate in.
   -> HailgunMessage -- ^ The Hailgun message to be sent.
   -> IO (Either HailgunErrorResponse HailgunSendResponse) -- ^ The result of the sent email. Either a sent email or a successful send.
sendEmail context message = do
   request <- postRequest url context (toEmailParts message)
   response <- httpLbs request =<< newManager tlsManagerSettings
   return $ parseResponse response
   where
      url = mailgunApiPrefixContext context ++ "/messages"

toEmailParts :: HailgunMessage -> [NCM.Part]
toEmailParts message = params ++ attachmentParts
   where
      params = map paramsToPart . toSimpleEmailParts $ message
      attachmentParts = map attachmentToPart . messageAttachments $ message

toSimpleEmailParts :: HailgunMessage -> [(BC.ByteString, BC.ByteString)]
toSimpleEmailParts message =
   [ (BC.pack "from", messageFrom message)
   , (BC.pack "subject", T.encodeUtf8 $ messageSubject message)
   -- we want to omit the `Sender` header to avoid shenanigans with some email
   -- clients when the `Sender` header and the `From` field don't originate from
   -- the same root domain. in some circumstances this caused the "reply"
   -- feature to break by attempting to use the `Sender`, which is
   -- undeliverable, instead of the `From` address. this magic header is
   -- undocumented and may break at literally any time. godspeed.
   , (BC.pack "h:X-Mailgun-Rewrite-Sender-Header", BC.pack "false")
   ] ++ to
   ++ cc
   ++ bcc
   ++ replyTo
   ++ inReplyTo
   ++ fromContent (messageContent message)
   where
      to = convertEmails (BC.pack "to") . messageTo $ message
      cc = convertEmails (BC.pack "cc") . messageCC $ message
      bcc = convertEmails (BC.pack "bcc") . messageBCC $ message
      replyTo = case messageReplyTo message of
         Nothing -> []
         Just email -> [(BC.pack "h:reply-to", email)]

      inReplyTo = case messageInReplyTo message of
         Nothing -> []
         Just email -> [(BC.pack "h:In-Reply-To", email)]

      fromContent :: MessageContent -> [(BC.ByteString, BC.ByteString)]
      fromContent t@(TextOnly _) = [ (BC.pack "text", textContent t) ]
      fromContent th@(TextAndHTML {}) = (BC.pack "html", htmlContent th) : fromContent (TextOnly . textContent $ th)

      convertEmails :: BC.ByteString -> [VerifiedEmailAddress] -> [(BC.ByteString, BC.ByteString)]
      convertEmails prefix = fmap ((,) prefix)

-- TODO replace with MailgunSendResponse
-- | The response to an email being accepted by the Mailgun API.
data HailgunSendResponse = HailgunSendResponse
   { hsrMessage :: String -- ^ The freeform message from the mailgun API.
   , hsrId      :: String -- ^ The ID of the message that has been accepted by the Mailgun api.
   }
   deriving (Show)

instance FromJSON HailgunSendResponse where
   parseJSON (Object v) = HailgunSendResponse
      <$> v .: T.pack "message"
      <*> v .: T.pack "id"
   parseJSON _ = mzero

