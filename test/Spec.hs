import           FromRequestTests     (fromRequestTests)
import           HelpFunctionTests    (functionsTests)
import           OperationsHandleTest (operationsTests)

main :: IO ()
main = do
    operationsTests
    functionsTests
    fromRequestTests
