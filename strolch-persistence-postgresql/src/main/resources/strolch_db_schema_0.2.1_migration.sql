
-- DB_VERSION
ALTER TABLE db_version ADD COLUMN app character varying(255);

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.2.1',
  'strolch',
  'Added new column app to table table version',
  CURRENT_TIMESTAMP
);
