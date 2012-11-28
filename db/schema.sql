-- schema

create table rooms (
  room_id serial primary key,
  created timestamp with time zone default now(),
  lat float,
  lng float
);

create table users (
  user_id serial primary key,
  name varchar default 'anon',
  room_id integer null references rooms(room_id) on delete cascade, 
  created timestamp with time zone default now(),
  exited timestamp with time zone 
);

create index users_room_id_idx on users (room_id);

create table messages (
  message_id serial primary key,
  user_id integer references users (user_id) on delete cascade,
  user_nick varchar,
  content text,
  room_id integer references rooms (room_id) on delete cascade,
  created timestamp with time zone default now()
);
create index messages_room_id_user_id_idx on messages (room_id, user_id);

select AddGeometryColumn('rooms', 'coordinates', 2163, 'POINT', 2);
select AddGeometryColumn('users', 'coordinates', 2163, 'POINT', 2);
select AddGeometryColumn('users', 'bounds', 2163, 'POLYGON', 2);

CREATE OR REPLACE FUNCTION update_room_geom() RETURNS trigger AS $$
BEGIN
  NEW.geom := ST_Transform(ST_GeomFromText('POINT(' || NEW.lng || ' ' || NEW.lat || ')', 4326), 2163);
  return NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_room_geom_trigger BEFORE INSERT ON rooms FOR EACH ROW EXECUTE PROCEDURE update_room_geom();

CREATE INDEX rooms_coordinates_idx  ON rooms  USING GIST (coordinates);
CREATE INDEX users_coordinates_idx  ON users  USING GIST (coordinates);
CREATE INDEX users_bounds_idx  ON users  USING GIST (bounds);

