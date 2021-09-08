select
  table_schema AS schema,
  table_name   AS table,
  column_name  AS column,
  replace(substr(pkt.ddl, POSITION('(' IN pkt.ddl)+1 ),')','') AS primary_key,
ordinal_position as column_order,
column_name,
data_type,
case when character_maximum_length is not null
then character_maximum_length
else numeric_precision end as max_length,
is_nullable,
column_default as default_value
from information_schema.columns
left join admin.v_generate_tbl_ddl pkt
ON pkt.tablename = table_name AND pkt.schemaname = table_schema
order by ordinal_position;
