
-- add new type
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'log_state_type') THEN
        CREATE TYPE log_state_type AS ENUM ('Active', 'Inactive', 'Information');
    END IF;
END$$;

-- add version columns
ALTER TABLE operations_log ADD COLUMN state log_state_type;

-- set initial values for new columns
UPDATE operations_log SET state = 'Information' where state IS NULL;

-- make columns not null
ALTER TABLE operations_log ALTER COLUMN state SET NOT NULL;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.0',
  'strolch',
  'Added log_state column to operations_log',
  CURRENT_TIMESTAMP
);