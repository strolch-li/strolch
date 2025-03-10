
-- update primary keys
ALTER TYPE access_type ADD VALUE IF NOT EXISTS 'EXECUTE';
ALTER TABLE audits ADD COLUMN additional_data json;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.5',
  'strolch',
  'extend audits with additional data and new access type',
  CURRENT_TIMESTAMP
);
