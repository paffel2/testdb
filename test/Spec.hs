import           FromRequestTests  (fromRequestTests)
import           HelpFunctionTests (functionsTests)
import           ImagesTests

--import OperationsHandleTest (operationsTests)
main :: IO ()
main
    --operationsTests
 = do
    functionsTests
    fromRequestTests
    imagesTests'
