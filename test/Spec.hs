import           AuthorsTests      (authorsTests)
import           FromRequestTests  (fromRequestTests)
import           HelpFunctionTests (functionsTests)
import           ImagesTests       (imagesTests)

--import OperationsHandleTest (operationsTests)
main :: IO ()
main
    --operationsTests
 = do
    functionsTests
    fromRequestTests
    imagesTests
    authorsTests
