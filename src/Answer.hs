module Answer where

import           Network.Wai (Request)

data AnswerHandle m a b =
    AnswerHandle
        { parseInformation  :: Request -> m a
        , databaseOperation :: a -> m b
        }

answer :: Monad m => Request -> AnswerHandle m a b -> m b
answer request handler =
    parseInformation handler request >>= databaseOperation handler
