-- schema

create table rooms (
  room_id serial primary key,
  created timestamp with time zone default now(),
  lat float,
  lng float
);

create table clients (
  client_id serial primary key,
  nickname varchar default 'anon',
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

select AddGeometryColumn('rooms', 'coordinates', 2163, 'POINT', 2);
select AddGeometryColumn('clients', 'coordinates', 2163, 'POINT', 2);
select AddGeometryColumn('clients', 'bounds', 2163, 'POLYGON', 2);

CREATE OR REPLACE FUNCTION update_room_geom() RETURNS trigger AS $$
BEGIN
  NEW.geom := ST_Transform(ST_GeomFromText('POINT(' || NEW.lng || ' ' || NEW.lat || ')', 4326), 2163);
  return NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS update_room_geom_trigger ON rooms;
CREATE TRIGGER update_room_geom_trigger BEFORE INSERT ON rooms FOR EACH ROW EXECUTE PROCEDURE update_room_geom();

CREATE INDEX rooms_coordinates_idx  ON rooms  USING GIST (coordinates);
CREATE INDEX clients_coordinates_idx  ON clients  USING GIST (coordinates);
CREATE INDEX clients_bounds_idx  ON clients  USING GIST (bounds);

