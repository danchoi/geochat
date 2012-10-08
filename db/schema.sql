
create table rooms (
  room_id serial primary key,
  created timestamp with time zone default now(),
  lat float,
  lng float
);

create table clients (
  client_id serial primary key,
  nickname varchar default 'anon',
  lat float,
  lng float,
  room_id integer null references rooms(room_id) on delete cascade, 
  created timestamp with time zone default now(),
  exited timestamp with time zone 
);

create index clients_room_id_idx on clients (room_id);

create table messages (
  message_id serial primary key,
  client_id integer references clients (client_id) on delete cascade,
  client_nick varchar,
  content text,
  room_id integer references rooms (room_id) on delete cascade,
  created timestamp with time zone default now()
);
create index messages_room_id_client_id_idx on messages (room_id, client_id);

