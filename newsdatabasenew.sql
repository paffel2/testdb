-- Database: NewsServer

-- DROP DATABASE "NewsServer";

CREATE DATABASE "NewsServer1"
    WITH 
    OWNER = postgres
    ENCODING = 'UTF8'
    LC_COLLATE = 'Russian_Russia.1251'
    LC_CTYPE = 'Russian_Russia.1251'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1;


CREATE TABLE Images (
image_id serial PRIMARY KEY,
image_name VARCHAR(50) NOT NULL,
image_b bytea NOT NULL);

CREATE TABLE Users (
    user_id serial PRIMARY KEY,
    first_name VARCHAR(20),
    last_name VARCHAR(20),
    avatar INT,
    login VARCHAR(20) UNIQUE NOT NULL,
    user_password VARCHAR(20) NOT NULL,
    creation_date DATE NOT NULL,
    admin_mark BOOLEAN NOT NULL,
    FOREIGN KEY (avatar) REFERENCES Images(image_id) ON DELETE SET NULL);

CREATE TABLE Authors (
    author_id serial PRIMARY KEY,
    author_user_id INT,
    description varchar(50),
    FOREIGN KEY (author_user_id) REFERENCES Users (user_id) ON DELETE SET NULL );

CREATE TABLE categories (
    category_id serial PRIMARY KEY,
    category_name varchar(50) NOT NULL UNIQUE,
    maternal_category int,
    FOREIGN KEY (maternal_category) REFERENCES categories(category_id) ON DELETE CASCADE);
    
    
CREATE TABLE Tags (
    tag_id serial PRIMARY KEY,
    tag_name varchar(20) NOT NULL UNIQUE);
    
CREATE TABLE News (
    news_id serial PRIMARY KEY,
    short_title varchar(20) NOT NULL,
    date_creation date NOT NULL,
    author_id int,
    category_id int NOT NULL,
    --tags int[],  убрать так как поиск неуодбен
    news_text text NOT NULL,
    main_image INT,
    --images bytea[], -- убрать так как поиск неуодбен
    FOREIGN KEY (author_id) REFERENCES authors (author_id) ON DELETE SET NULL,
    FOREIGN KEY (category_id) REFERENCES categories (category_id) ON DELETE CASCADE,
    FOREIGN KEY (main_image) REFERENCES Images (image_id) ON DELETE SET NULL
    );

CREATE TABLE Users_comments (
    comment_id serial PRIMARY KEY,
    user_id int NOT NULL,
    comment_text text NOT NULL,
    news_id int NOT NULL,
    comment_time timestamp NOT NULL,
    FOREIGN KEY (user_id) REFERENCES Users (user_id) ON DELETE CASCADE,
    FOREIGN KEY (news_id) REFERENCES News (news_id) ON DELETE CASCADE);

CREATE TABLE Drafts (
    draft_id serial PRIMARY KEY,
    news_id int,
    user_id int NOT NULL,
    short_title varchar(20),
    date_of_changes date NOT NULL,
    category_id int,
    draft_text text,
    main_image INT,
    FOREIGN KEY (user_id) REFERENCES Users (user_id) ON DELETE CASCADE,
    FOREIGN KEY (news_id) REFERENCES News (news_id) ON DELETE SET NULL,
    FOREIGN KEY (category_id) REFERENCES categories (category_id) ON DELETE SET NULL,
    FOREIGN KEY (main_image) REFERENCES Images (image_id) ON DELETE SET NULL); 


CREATE TABLE News_tags (
    news_id int NOT NULL,
    tag_id int NOT NULL,
    FOREIGN KEY (news_id) REFERENCES News (news_id) ON DELETE CASCADE,
    FOREIGN KEY (tag_id) REFERENCES Tags (tag_id) ON DELETE CASCADE);

/*CREATE TABLE News_images (
    news_id int,
    image bytea,
    image_name VARCAHR(50),
    FOREIGN KEY (news_id) REFERENCES News (news_id) ON DELETE CASCADE);

CREATE TABLE Drafts_images (
    draft_id int,
    image bytea,
    image_name VARCAHR(50),
    FOREIGN KEY (draft_id) REFERENCES Drafts (draft_id) ON DELETE CASCADE);*/
CREATE TABLE News_images (
    news_id int NOT NULL,
    image_id int NOT NULL,
    FOREIGN KEY (news_id) REFERENCES News (news_id) ON DELETE CASCADE,
    FOREIGN KEY (image_id) REFERENCES Images (image_id) ON DELETE CASCADE);

CREATE TABLE Drafts_images (
    draft_id int NOT NULL,
    image_id INT NOT NULL,
    --image_name VARCAHR(50),
    FOREIGN KEY (draft_id) REFERENCES Drafts (draft_id) ON DELETE CASCADE,
    FOREIGN KEY (image_id) REFERENCES Images (image_id) ON DELETE CASCADE);

CREATE TABLE Draft_tags (
    draft_id int NOT NULL,
    tag_id int NOT NULL,
    FOREIGN KEY (draft_id) REFERENCES Drafts (draft_id) ON DELETE CASCADE,
    FOREIGN KEY (tag_id) REFERENCES Tags (tag_id) ON DELETE CASCADE);

CREATE TABLE Tokens (
	user_id int NOT NULL UNIQUE PRIMARY KEY, 
	token VARCHAR(50) NOT NULL UNIQUE,
	creation_date timestamp with time zone NOT NULL,
	FOREIGN KEY (user_id) REFERENCES Users (user_id) ON DELETE CASCADE);

CREATE EXTENSION pgcrypto;

 /*
select news_id, title, array_agg(image_b) as images_b from images right join news USING(news_id)
group by 1
order by news_id*/   

/*insert into USERS(login,password)
 values ('dahaku',crypt('1234',gen_salt('des')))
 
select (password = crypt('123',password)) as check_pass from
users where login = 'dahaku'*/
