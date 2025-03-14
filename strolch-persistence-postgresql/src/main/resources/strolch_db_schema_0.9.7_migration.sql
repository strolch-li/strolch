
-- update primary keys
ALTER TABLE audits ALTER COLUMN element_accessed TYPE varchar;
ALTER TABLE audits ALTER COLUMN action TYPE varchar;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.7',
  'strolch',
  'remove length on columns element_accessed and action',
  CURRENT_TIMESTAMP
);
