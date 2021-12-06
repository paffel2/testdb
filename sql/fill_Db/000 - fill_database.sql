CREATE TABLE Images (
image_id serial PRIMARY KEY,
image_name VARCHAR(50) NOT NULL,
image_b bytea NOT NULL,
content_type VARCHAR(50) NOT NULL);

CREATE TABLE Users (
    user_id serial PRIMARY KEY,
    first_name VARCHAR(20),
    last_name VARCHAR(20),
    avatar INT,
    login VARCHAR(20) UNIQUE NOT NULL,
    user_password VARCHAR(60) NOT NULL,
    creation_date TIMESTAMP WITH TIME ZONE NOT NULL,
    admin_mark BOOLEAN NOT NULL);

CREATE TABLE Authors (
    author_id serial PRIMARY KEY,
    user_id INT UNIQUE NOT NULL,
    description varchar(50));

CREATE TABLE categories (
    category_id serial PRIMARY KEY,
    category_name varchar(50) NOT NULL UNIQUE,
    maternal_category int);


CREATE TABLE Tags (
    tag_id serial PRIMARY KEY,
    tag_name varchar(20) NOT NULL UNIQUE);

CREATE TABLE News (
    news_id serial PRIMARY KEY,
    short_title varchar(20) NOT NULL,
    date_creation DATE NOT NULL,
    author_id int,
    category_id int NOT NULL,
    news_text text NOT NULL,
    main_image INT);

CREATE TABLE Users_comments (
    comment_id serial PRIMARY KEY,
    user_id int NOT NULL,
    comment_text text NOT NULL,
    news_id int NOT NULL,
    comment_time TIMESTAMP WITH TIME ZONE NOT NULL);

CREATE TABLE Drafts (
    draft_id serial PRIMARY KEY,
    author_id int NOT NULL,
    short_title varchar(20),
    date_of_changes TIMESTAMP WITH TIME ZONE NOT NULL,
    category_id int,
    draft_text text,
    main_image INT);


CREATE TABLE News_tags (
    news_id int NOT NULL,
    tag_id int NOT NULL);


CREATE TABLE News_images (
    news_id int NOT NULL,
    image_id int NOT NULL);

CREATE TABLE Drafts_images (
    draft_id int NOT NULL,
    image_id INT NOT NULL);

CREATE TABLE Draft_tags (
    draft_id int NOT NULL,
    tag_id int NOT NULL);

CREATE TABLE Tokens (
	user_id int NOT NULL UNIQUE PRIMARY KEY, 
	token VARCHAR(50) NOT NULL UNIQUE,
	creation_date timestamp with time zone NOT NULL);

CREATE EXTENSION pgcrypto;

create function take_categories_list(cat_id int) returns text as $$
	with recursive included_categories(category_id,category_name, maternal_category) as 
	(select category_id,category_name, maternal_category from categories where category_id = cat_id
	union all
	select c.category_id,c.category_name,c.maternal_category from
 	included_categories i, categories c
 	where c.category_id = i.maternal_category)
	select replace(replace(concat_ws(', ',array_agg(category_name)),'{',''),'}','') from included_categories $$
	LANGUAGE SQL
    IMMUTABLE
	STRICT;

create function check_token (token_in varchar(50),interval_in int ) returns int as $$
    select user_id from tokens where token = token_in and (current_timestamp - tokens.creation_date < make_interval(secs => interval_in)) $$
    LANGUAGE SQL
    IMMUTABLE
	STRICT;

create function isAdmin (token_in varchar(50), interval_in int) 
    returns BOOLEAN AS $$ 
    declare admin_token boolean;
    BEGIN
	    select admin_mark into admin_token from users join tokens using (user_id) where token = token_in
	    and ((current_timestamp - tokens.creation_date) < make_interval(secs => interval_in));
	    if admin_token is null then 
		    return False;
	    else 
		    return admin_token;
	    end if;
    END; $$
    LANGUAGE plpgsql
    IMMUTABLE
    STRICT;