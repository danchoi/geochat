dir=/usr/share/postgresql/9.1/contrib/postgis-2.0
psql -d geochat -f $dir/postgis.sql
psql -d geochat -f $dir/spatial_ref_sys.sql 
psql -d geochat -f $dir/postgis_comments.sql 


