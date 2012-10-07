dropdb geochat
createdb geochat
psql -d geochat -f db/schema.sql
bash db/postgis.sh
psql -d geochat -f db/schema2.sql

