
-- add version columns
ALTER TABLE activities ADD COLUMN state order_state;

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.5.1',
  'strolch',
  'Added state column to activity',
  CURRENT_TIMESTAMP
);
