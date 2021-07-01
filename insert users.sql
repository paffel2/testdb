insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Павел', 'Зарубин',3, 'dahaku',crypt('qwerty',gen_salt('md5')),current_date,TRUE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Ярослав', 'Новиков',3, 'niamh',crypt('qwerty1',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Артур', 'Дьячков',4, 'phir',crypt('qwerty2',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Цезарь', 'Русаков',5, 'josethai',crypt('qwerty3',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Савва', 'Федосеев',6, 'mirosha',crypt('qwerty4',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Иван', 'Новиков',7, 'uman',crypt('qwerty5',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Вадим', 'Трясило',8, 'ylan',crypt('qwerty6',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Устин', 'Беляев',9, 'ppoleg',crypt('qwerty7',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Эдуард', 'Вишняков',10, 'cybelin',crypt('qwerty8',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Аполлон', 'Ерёменко',11, 'xieana',crypt('qwerty9',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Чарльз', 'Матвеев',12, 'zantony',crypt('qwerty10',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Леон', 'Исаев',13, 'hawkaaa',crypt('qwerty11',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Никита', 'Борисенко',14, 'lannalie',crypt('qwerty12',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Ждан', 'Мухин',15, 'charl',crypt('qwerty13',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Макар', 'Андрейко',16, 'cianal',crypt('qwerty14',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Евсей', 'Пономаренко',17, 'helm',crypt('qwerty15',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Людовик', 'Сафонов',18, 'nahal',crypt('qwerty16',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Тит', 'Фадеев',19, 'soneli',crypt('qwerty17',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Цезарь', 'Иващенко',20, 'hell',crypt('qwerty18',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Корнелий', 'Миклашевский',21, 'tamal',crypt('qwerty19',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Юрий', 'Наумов',22, 'taxian',crypt('qwerty20',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Платон', 'Орлов',23, 'xadr',crypt('qwerty21',gen_salt('md5')),current_date,FALSE);

insert into users (first_name, last_name, avatar, login, user_password,creation_date,admin_mark)
values ('Клим', 'Сирко',24, 'alenal',crypt('qwerty22',gen_salt('md5')),current_date,FALSE);

select login, image_name from users join images
on users.avatar = images.image_id;
