{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Answer
import           Answers.Drafts
import           Answers.Users
import           Control.Monad.Except
import           Controllers.Authors         (authorsRouter)
import           Controllers.Categories      (categoriesRouter)
import           Controllers.Drafts          (draftsRouter)
import           Controllers.Images          (imagesRouter)
import           Controllers.NewsAndComments (newsAndCommentsRouter)
import           Controllers.Tags            (tagsRouter)
import qualified Data.ByteString.Char8       as BC
import           Network.Wai                 (Request (rawPathInfo), Response)
import           OperationsHandle
import           Responses                   (toResponse)
import           Types.Other                 (ResponseErrorMessage (NotFound),
                                              ResponseOkMessage, SomeError)

{-routes ::
       Monad m
    => OperationsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)-}
routes ::
       OperationsHandle (ExceptT SomeError IO) IO
    -> Request
    -> IO (Either ResponseErrorMessage ResponseOkMessage)
routes operations req =
    case pathHead
        --"news" -> newsAndCommentsRouter (newsAndCommentsHandle operations) req
          of
        "login" -> answer' req (signInHandle $ usersHandle operations)
        "registration" --answer' req (registrationHandle' usersHandler' (Pool Connection) (LoggerHandle IO) TokenLifeTime )
         -> answer' req (registrationHandle $ usersHandle operations)
        "deleteUser" -> answer' req (deleteUserHandle $ usersHandle operations)
        --"categories" -> categoriesRouter (categoriesHandle operations) req
        "profile" -> answer' req (profileUserHandle $ usersHandle operations)
        --"drafts" -> draftsRouter (draftsHandle operations) req
        --"new_draft" -> answer req (createDraftHandle $ draftsHandle operations)
        --"tags" -> tagsRouter (tagsHandle operations) req
        --"image" -> imagesRouter (imagesHandle operations) req
        --"authors" -> authorsRouter (authorsHandle operations) req
        _ -> return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems

responder ::
       Monad m => Either ResponseErrorMessage ResponseOkMessage -> m Response
responder something = return $ toResponse something

{-application ::
       Monad m => OperationsHandle m -> Request -> (Response -> m b) -> m b-}
application ::
       OperationsHandle (ExceptT SomeError IO) IO
    -> Request
    -> (Response -> IO b)
    -> IO b
application operations req respond =
    routes operations req >>= responder >>= respond
