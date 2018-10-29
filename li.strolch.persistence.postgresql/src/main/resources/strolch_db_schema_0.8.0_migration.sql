
-- add version columns
ALTER TABLE resources ADD COLUMN updated_at timestamp with time zone;
ALTER TABLE orders ADD COLUMN updated_at timestamp with time zone;
ALTER TABLE activities ADD COLUMN updated_at timestamp with time zone;

-- set initial values for new columns
UPDATE resources SET updated_at = CURRENT_TIMESTAMP where updated_at IS NULL;
UPDATE orders SET updated_at = CURRENT_TIMESTAMP where updated_at IS NULL;
UPDATE activities SET updated_at = CURRENT_TIMESTAMP where updated_at IS NULL;

-- make columns not null
ALTER TABLE resources ALTER COLUMN updated_at SET NOT NULL;
ALTER TABLE orders ALTER COLUMN updated_at SET NOT NULL;
ALTER TABLE activities ALTER COLUMN updated_at SET NOT NULL;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.8.0',
  'strolch',
  'Added updated_at column to all tables',
  CURRENT_TIMESTAMP
);
