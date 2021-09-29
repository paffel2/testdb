# testdb
This repository contains a simple web news server. The project was created as a test task of the Metalamp company. 
# Installation and Startup Guide
To run and install the program, you must install the [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
After installing stack, clone this repository. Install the compiler (if required) and build the project.

    # install compiler
      stack setup
    # build project
      stack build
## Configuration file
    token {
            lifetime = 86400 
          }
    database {
        host = "localhost"
        port = "5432"
        login = "login"
        password = "password"
        name = "ServerNews"
             }
    logger {
        priority = "Debug"/"Info"/"Warning"/"Error"
           }
    server {
        port = 8000
        maximum_body_flush = 1
           }
    pool {
        num_stripes = 1
        idle_time = 40
        max_resources = 10
         }
         
Before the run, you must create database and run postgresql server. 

For Linux:

    sudo service postgresql start - for starting postgresql server;
    sudo su - postgres - for changing user;
    createdb -D pg_default -E UTF8 -l en_US.UTF-8 -T template0 -O <login> <data_base_name>  - for creating new database.

For Windows:

    createdb -U <login> -D pg_default -E UTF8 -l Russian_Russia.1251 -O <login> <data_base_name>

To the run server use 

    stack exec testdb-exe

# Project structure

* `app/Main.hs` - Preparing the server settings and launch.
* `sh/` - contains sh-scripts for testing.
* `sql/` - contains scripts for filling database with test data.
* `src/` - contains all code used by programm.
    * `Controllers` - controllers and routers of entities and additional commands.
    * `Databaseoperations` - functions related to editing and retrieving data from the database.
    * `Config.hs` - contains operations related to server settings.
    * `ControllersHandle.hs` - contains a handle using all the necessary controllers.
    * `FromRequest.hs` - contains functions to extract data from received requests.
    * `HelpFunctions.hs` - contains various functions for the operation of other modules.
    * `Logger.hs` - contains the logger implementation.
    * `PostgreSqlWithPool.hs` - contains functions for working with the database.
    * `Responses.hs` - contains the responses sent by the server.
    * `Router.hs` - contains the main router.
    * `Types.hs` - contains types and models used in work.
* `test` - contains tests.
* `server.conf` - server's config file.


#Endpoints

* `News endpoints`
    * `news` - sending lists containts ten news.
    * `news/id` - sending news with id.
    * `news/id/comments` - sending a list of comments to the news.
    * `news/id/comments/add_comment` - adding a comment.
    * `news/id/comments/delete_comment` - deleting comment.
* `Authors endpoints`
    * `authors` - sending list of authors.
    * `authors/delete_author` - deleting author.
    * `authors/create_author` - creating new author.
    * `authors/edit_author` - editing author description.
* `Categories endpoints`
    * `categories` - sending list of categories.
    * `categories/delete_category` - deleting category.
    * `categories/create_category` - creating new category.
    * `categories/edit_category` - editing category.

* `Drafts endpoints`
    * `drafts` - sending list of author's drafts.
    * `drafts/id` - sending draft with id.
    * `drafts/id/delete_draft` - deleting category.
    * `drafts/id/update_draft` - editing draft.
    * `drafts/id/public_news` - public draft as news.
    * `new_draft` - creating draft
* `Tags endpoints`
    * `tags` - sending list of tags.
    * `tags/delete_tag` - deleting tag.
    * `tags/create_tag` - creating new tag.
    * `tags/edit_tag` - editing tag.

* `Images endpoints`
    * `image` - sending list of image's names.
    * `image/id` - sending image.
* `Users endpoints`
    * `registaration` - registration new user.
    * `login` - sign in user and token update.
    * `deleteUser` - deleting user.
    * `profile` - sending user information.

* `Initialization endpoint`
    * `initDb` - initialization filling test data to database.


