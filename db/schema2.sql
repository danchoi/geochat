select AddGeometryColumn('rooms', 'geom', 2163, 'POINT', 2);
select AddGeometryColumn('clients', 'geom', 2163, 'POINT', 2);

CREATE OR REPLACE FUNCTION update_room_geom() RETURNS trigger AS $$
BEGIN
  NEW.geom := ST_Transform(ST_GeomFromText('POINT(' || NEW.lng || ' ' || NEW.lat || ')', 4326), 2163);
  return NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS update_room_geom_trigger ON rooms;
CREATE TRIGGER update_room_geom_trigger BEFORE INSERT ON rooms FOR EACH ROW EXECUTE PROCEDURE update_room_geom();


