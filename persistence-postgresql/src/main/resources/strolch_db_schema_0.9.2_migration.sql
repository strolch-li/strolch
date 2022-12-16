
-- create index
DROP INDEX IF EXISTS ids_orders_date;
CREATE INDEX ids_orders_date
    ON orders (date NULLS LAST)
;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.2',
  'strolch',
  'create index on date orders table',
  CURRENT_TIMESTAMP
);
