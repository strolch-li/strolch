
-- update primary keys
ALTER TYPE log_severity_type ADD VALUE IF NOT EXISTS 'System';

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.4',
  'strolch',
  'add log severity type value System',
  CURRENT_TIMESTAMP
);
