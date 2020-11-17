
-- add version columns
ALTER TABLE archive_orders ADD COLUMN updated_at timestamp with time zone;

-- set initial values for new columns
UPDATE archive_orders SET updated_at = CURRENT_TIMESTAMP where updated_at IS NULL;

-- make columns not null
ALTER TABLE archive_orders ALTER COLUMN updated_at SET NOT NULL;


INSERT INTO db_version
  (version, app, description, created)
values(
  '0.2.0',
  'archive',
  'Added updated_at column',
  CURRENT_TIMESTAMP
);
