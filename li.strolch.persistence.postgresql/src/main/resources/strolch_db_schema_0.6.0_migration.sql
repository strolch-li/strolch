
-- Convert to new type, casting via text representation
ALTER TABLE orders ADD COLUMN asjson json;
ALTER TABLE orders ALTER COLUMN asxml DROP NOT NULL;

ALTER TABLE resources ADD COLUMN asjson json;
ALTER TABLE resources ALTER COLUMN asxml DROP NOT NULL;

ALTER TABLE activities ADD COLUMN asjson json;
ALTER TABLE activities ALTER COLUMN asxml DROP NOT NULL;

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.6.0',
  'strolch',
  'Added json column to all tables',
  CURRENT_TIMESTAMP
);
