
-- update primary keys
ALTER TABLE audits DROP COLUMN firstname;
ALTER TABLE audits DROP COLUMN lastname;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.6',
  'strolch',
  'remove audit columns firstname and lastname',
  CURRENT_TIMESTAMP
);
