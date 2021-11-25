--import           AuthorsTests      (authorsTests)
--import           CategoriesTests   (categoriesTests)
--import           CommentsTests     (commentsTests)
--import           DraftsTests       (draftsTests)
import           FromRequestTests  (fromRequestTests)
import           HelpFunctionTests (functionsTests)
import           ImagesTests       (imagesTests)

--import           NewsTests         (newsTests)
import           TagsTests         (tagsTests)
import           UsersTests        (usersTests)

main :: IO ()
main = do
    functionsTests
    fromRequestTests
    imagesTests
    --authorsTests
    --categoriesTests
    tagsTests
    usersTests
    --draftsTests
    --commentsTests
    --newsTests
