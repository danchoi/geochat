
create table rooms (
  room_id serial primary key,
  created timestamp with time zone default now(),
  lat float,
  lng float
);

create table clients (
  client_id serial primary key,
  nickname varchar unique not null,
  room_id integer null references rooms(room_id),
  created timestamp with time zone default now()
);