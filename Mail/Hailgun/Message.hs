module Mail.Hailgun.Message
    ( hailgunMessage
    , hailgunMessageWithReplyTo
    ) where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.ByteString.Char8 as BC
import Data.List (find)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Mail.Hailgun.Attachment.Internal
import Mail.Hailgun.AttachmentsSearch
import Mail.Hailgun.Internal.Data
import Text.Email.Validate

-- | A method to construct a HailgunMessage. You require a subject, content, From address and people
-- to send the email to and it will give you back a valid Hailgun email message. Or it will error
-- out while trying.
hailgunMessage
   :: MessageSubject -- ^ The purpose of the email surmised.
   -> MessageContent -- ^ The full body of the email.
   -> UnverifiedEmailAddress -- ^ The email account that the recipients should respond to in order to get back to us.
   -> MessageRecipients -- ^ The people that should recieve this email.
   -> [Attachment] -- ^ The attachments that you want to attach to the email; standard or inline.
   -> Either HailgunErrorMessage HailgunMessage -- ^ Either an error while trying to create a valid message or a valid message.
hailgunMessage subject content sender recipients simpleAttachments =
   hailgunMessageWithReplyTo subject content sender recipients simpleAttachments Nothing

-- | A method to construct a HailgunMessage with a Reply-To custom header. Right now the "Reply-To"
-- part is hardcoded, if we need to extend hailgun with more custom headers we should switch to a
-- list of (name, value) pairs.
hailgunMessageWithReplyTo
   :: MessageSubject
   -> MessageContent
   -> UnverifiedEmailAddress
   -> MessageRecipients
   -> [Attachment]
   -> Maybe UnverifiedEmailAddress
   -> Either HailgunErrorMessage HailgunMessage
hailgunMessageWithReplyTo subject content sender recipients simpleAttachments replyTo = do
   from  <- validateRecipient sender
   to    <- mapM validateRecipient (recipientsTo recipients)
   cc    <- mapM validateRecipient (recipientsCC recipients)
   bcc   <- mapM validateRecipient (recipientsBCC recipients)
   attachments <- attachmentsInferredFromMessage content cleanAttachments
   replyToAddr <- validateRecipient `traverse` replyTo
   return HailgunMessage
      { messageId = Nothing
      , messageSubject = subject
      , messageContent = content
      , messageFrom = from
      , messageTo = to
      , messageCC = cc
      , messageBCC = bcc
      , messageAttachments = attachments
      , messageReplyTo = replyToAddr
      , messageInReplyTo = Nothing
      , messageReferences = Nothing
      , messageTags = []
      , messageRecipientVariables = mempty
      , messageDeliveryTime = Nothing
      , messageTestMode = False
      }
   where
      cleanAttachments = fmap cleanAttachmentFilePath simpleAttachments

extractEmail :: T.Text -> Either String T.Text
extractEmail = parseOnly
   $ many (satisfy (/= '<')) *> char '<' *> (T.pack <$> many (satisfy (/= '>')) <* char '>')
   <|> T.pack <$> many anyChar

validateEmail :: T.Text -> Either String EmailAddress
validateEmail e = validate . encodeUtf8 =<< extractEmail e

validateRecipient :: UnverifiedEmailAddress -> Either String VerifiedEmailAddress
validateRecipient ue = ue <$ validateEmail (decodeUtf8 ue)

attachmentsInferredFromMessage :: MessageContent -> [Attachment] -> Either String [SpecificAttachment]
attachmentsInferredFromMessage mContent simpleAttachments =
   case mContent of
      (TextOnly _) -> return . fmap toStandardAttachment $ simpleAttachments
      th@(TextAndHTML {}) -> convertAttachments simpleAttachments (findInlineImagesInHtmlEmail . htmlContent $ th)
      (Template _) ->  return . fmap toStandardAttachment $ simpleAttachments

convertAttachments :: [Attachment] -> [InlineImage] -> Either String [SpecificAttachment]
convertAttachments attachments images = do
   inlineAttachments <- sequence (fmap (findAttachmentForImage attachments) images)
   let standardAttachments = toStandardAttachment <$> attachments `notInSpecific` inlineAttachments
   return $ inlineAttachments ++ standardAttachments

notInSpecific :: [Attachment] -> [SpecificAttachment] -> [Attachment]
notInSpecific simpleAttachments specificAttachments =
   filter (\sa -> attachmentFilePath sa `notElem` specificFilePaths) simpleAttachments
   where
      specificFilePaths = fmap saFilePath specificAttachments

findAttachmentForImage :: [Attachment] -> InlineImage -> Either String SpecificAttachment
findAttachmentForImage attachments image =
   case find (`attachmentForInlineImage` image) attachments of
      Nothing -> Left . missingInlineImageErrorMessage $ image
      Just attachment -> Right . toInlineAttachment $ attachment

missingInlineImageErrorMessage :: InlineImage -> String
missingInlineImageErrorMessage image =
   "Could not find an attachment for the inline image: "
   ++ (show . imageSrc $ image)
   ++ ". Either provide the attachment or remove the inline image from the HTML email."

attachmentForInlineImage :: Attachment -> InlineImage -> Bool
attachmentForInlineImage attachment image = (BC.pack . attachmentFilePath $ attachment) == imageSrc image
