
select AddGeometryColumn('rooms', 'geom', 2163, 'POINT', 2);
select AddGeometryColumn('clients', 'geom', 2163, 'POINT', 2);

update rooms set geom = ST_Transform( ST_GeomFromText('POINT(' || lng || ' ' || lat  || ')', 4326), 2163 );

select 
  client_id,
  clients.lat as client_lat,
  clients.lng as client_lat,
  rooms.room_id, 
  rooms.lat as room_lat, 
  rooms.lng as room_lng, 
  ST_Distance(
   ST_Transform( ST_GeomFromText('POINT(' || clients.lng || ' ' || clients.lat  || ')', 4326), 2163 ), rooms.geom) dist
from clients
  inner join rooms using(room_id) ;

select rooms.room_id, rooms.lat, rooms.lng, ST_Distance(ST_Transform( ST_GeomFromText('POINT(' || 43.36 || ' ' || -71.120 || ')', 4326), 2163 ), rooms.geom) dist from rooms where lat is not null and lng is not null order by dist desc limit 2; 

select rooms.room_id, rooms.lat, rooms.lng, ST_Distance(ST_Transform( ST_GeomFromText('POINT(' || -69.14460699999999 || ' ' || 42.3639096  || ')', 4326), 2163 ), rooms.geom) dist from rooms where ST_Distance(ST_Transform( ST_GeomFromText('POINT(' || -70.14460699999999 || ' ' || 42.3639096  || ')', 4326), 2163 ), rooms.geom) is not null order by dist asc;

