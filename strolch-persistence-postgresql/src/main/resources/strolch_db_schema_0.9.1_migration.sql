
-- add bundle column
ALTER TABLE operations_log ADD COLUMN bundle varchar(255);

-- set initial values for new columns
UPDATE operations_log SET bundle = '' where bundle IS NULL;

-- make columns not null
ALTER TABLE operations_log ALTER COLUMN bundle SET NOT NULL;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.1',
  'strolch',
  'Added bundle column to operations_log',
  CURRENT_TIMESTAMP
);
