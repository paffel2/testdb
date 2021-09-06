import FromRequestTests (fromRequestTests)
import HelpFunctionTests (functionsTests)

main :: IO ()
main = do
    functionsTests
    fromRequestTests
