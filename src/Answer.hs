module Answer where

import           Network.Wai (Request)
import           Types.Other (ResponseErrorMessage, ResponseOkMessage,
                              SomeError)

data AnswerHandle m a b =
    AnswerHandle
        { parseInformation :: Request -> m a
        , databaseOperation :: a -> m (Either SomeError b)
        , sendResult :: Either SomeError b -> m (Either ResponseErrorMessage ResponseOkMessage)
        }

answer ::
       Monad m
    => Request
    -> AnswerHandle m a b
    -> m (Either ResponseErrorMessage ResponseOkMessage)
answer request handler =
    parseInformation handler request >>= databaseOperation handler >>=
    sendResult handler

data AnswerHandle' m a b io =
    AnswerHandle'
        { parseInformation' :: Request -> m a
        , databaseOperation' :: a -> m b
        , sendResult' :: m b -> io (Either ResponseErrorMessage ResponseOkMessage)
        }

answer' ::
       (Monad m, Monad io)
    => Request
    -> AnswerHandle' m a b io
    -> io (Either ResponseErrorMessage ResponseOkMessage)
answer' request handler = do
    let result =
            parseInformation' handler request >>= databaseOperation' handler
    sendResult' handler result

data AnswerHandle'' m a b =
    AnswerHandle''
        { parseInformation''  :: Request -> m a
        , databaseOperation'' :: a -> m b
        --, sendResult' :: m b -> io (Either ResponseErrorMessage ResponseOkMessage)
        }

answer'' :: Monad m => Request -> AnswerHandle'' m a b -> m b
answer'' request handler =
    parseInformation'' handler request >>= databaseOperation'' handler
