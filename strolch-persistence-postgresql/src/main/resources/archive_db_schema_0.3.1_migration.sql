
-- create index
DROP INDEX IF EXISTS ids_archive_orders_date;
CREATE INDEX ids_archive_orders_date
    ON archive_orders (date NULLS LAST)
;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.3.1',
  'archive',
  'create index on date archive_orders table',
  CURRENT_TIMESTAMP
);
