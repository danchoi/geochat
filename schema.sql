
create table clients (
  client_id serial primary key,
  nickname varchar unique not null,
  created timestamp with time zone default now()
);
