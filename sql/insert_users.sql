insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('John', 'Smith',1, 'niamh',crypt('qwerty1',gen_salt('md5')),current_date,TRUE);

insert into tokens(user_id,token,creation_date)
values (1,'qwerty1',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Kristian', 'Wilcox',2, 'phir',crypt('qwerty2',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (2,'qwerty2',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Owen', 'Cook',3, 'josethai',crypt('qwerty3',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (3,'qwerty3',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Damon', 'Page',4, 'mirosha',crypt('qwerty4',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (4,'qwerty4',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Victor', 'Lang',5, 'uman',crypt('qwerty5',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (5,'qwerty5',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Harry', 'Wells',6, 'ylan',crypt('qwerty6',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (6,'qwerty6',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Jeremy', 'Small',7, 'ppoleg',crypt('qwerty7',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (7,'qwerty7',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Percival', 'Lane',8, 'cybelin',crypt('qwerty8',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (8,'qwerty8',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Augustus', 'Hudson',9, 'xieana',crypt('qwerty9',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (9,'qwerty9',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Scott', 'Reynolds',10, 'zantony',crypt('qwerty10',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (10,'qwerty10',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Johnathan', 'Barker',11, 'hawkaaa',crypt('qwerty11',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (11,'qwerty11',now());

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Basil', 'Bell',12, 'lannalie',crypt('qwerty12',gen_salt('md5')),current_date,FALSE);

insert into tokens(user_id,token,creation_date)
values (12,'qwerty12',now());