import           AuthorsTests      (authorsTests)
import           CategoriesTests   (categoriesTests)
import           FromRequestTests  (fromRequestTests)
import           HelpFunctionTests (functionsTests)
import           ImagesTests       (imagesTests)
import           TagsTests         (tagsTests)
import           UsersTests        (usersTests)

--import OperationsHandleTest (operationsTests)
main :: IO ()
main
    --operationsTests
 = do
    functionsTests
    fromRequestTests
    imagesTests
    authorsTests
    categoriesTests
    tagsTests
    usersTests
