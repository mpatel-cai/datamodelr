SELECT
  c.table_schema AS "schema",
  t.table_name AS "table",
  c.column_name AS "column",
  CASE WHEN pk.column_name is null then 0 else 1 end AS "key",
  fk.ref,
  fk.ref_col,
  CASE c.is_nullable when 'YES' then 0 else 1 end AS "mandatory",
  c.data_type AS "type",
  c.ordinal_position AS column_order

FROM
  (SELECT * FROM information_schema.columns WHERE table_schema = '{schema}') c
  inner join information_schema.tables t on
    t.table_name = c.table_name
    and t.table_catalog = c.table_catalog
    and t.table_schema = c.table_schema

  left join  -- primary keys
  ( SELECT
      tc.constraint_name, tc.table_name, kcu.column_name
      FROM
      information_schema.table_constraints AS tc
      JOIN information_schema.key_column_usage AS kcu ON
      tc.constraint_name = kcu.constraint_name
    WHERE constraint_type = 'PRIMARY KEY'
  ) pk on
    pk.table_name = c.table_name
    and pk.column_name = c.column_name

  left join  -- foreign keys
    ( SELECT
        tc.constraint_name, kcu.table_name, kcu.column_name,
        ccu.table_name AS "ref",
        ccu.column_name AS "ref_col"
      FROM
        information_schema.table_constraints AS tc
        JOIN information_schema.key_column_usage AS kcu ON
        tc.constraint_name = kcu.constraint_name
        JOIN information_schema.constraint_column_usage AS ccu ON
        ccu.constraint_name = tc.constraint_name
      WHERE tc.constraint_type = 'FOREIGN KEY'
    ) fk on
      fk.table_name = c.table_name
      and fk.column_name = c.column_name

where t.table_type = 'BASE TABLE';
