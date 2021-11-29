{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors where

import           Answer                (answer)
import           Answers.Authors       (authorsListHandle, createAuthorHandle,
                                        deleteAuthorHandle, updateAuthorHandle)
import           Control.Monad.Except  (ExceptT, MonadIO)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import           HelpFunction          (sendResult)
import           Logger                (LoggerHandle)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (AuthorsHandle)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError)

authorsRouter ::
       MonadIO m
    => AuthorsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
authorsRouter methods hLogger req
    | pathElemsC == 1 =
        sendResult hLogger "List of authors not sended." authorsOk $
        answer req (authorsListHandle methods)
    | pathElemsC == 2 =
        case last pathElems of
            "delete_author" ->
                sendResult hLogger "Author not deleted." deleteAuthorsOk $
                answer req (deleteAuthorHandle methods)
            "create_author" ->
                sendResult hLogger "Author not created." createAuthorsOk $
                answer req (createAuthorHandle methods)
            "edit_author" ->
                sendResult hLogger "Author not edited." updateAuthorOk $
                answer req (updateAuthorHandle methods)
            _ -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
    authorsOk a = OkJSON . encode $ a
    deleteAuthorsOk _ = OkMessage "Author deleted."
    createAuthorsOk n = Created $ LBS.fromStrict $ BC.pack $ show n
    updateAuthorOk _ = OkMessage "Author edited."
