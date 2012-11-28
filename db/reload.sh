dropdb geochat
createdb geochat

os=`uname`
echo $os
if [[ "$os" == 'Darwin' ]]; then
  dir=/usr/local/Cellar/postgis/2.0.1/share/postgis
else
  dir=/usr/share/postgresql/9.1/contrib/postgis-1.5
fi
echo Using postgis dir: $os
sleep 2
psql -d geochat -f $dir/postgis.sql
psql -d geochat -f $dir/spatial_ref_sys.sql 
psql -d geochat -f $dir/postgis_comments.sql 



psql -d geochat -f db/schema.sql
