
-- add version columns
ALTER TABLE resources ADD COLUMN version integer;
ALTER TABLE resources ADD COLUMN created_by varchar(255);
ALTER TABLE resources ADD COLUMN created_at timestamp with time zone;
ALTER TABLE resources ADD COLUMN deleted boolean;
ALTER TABLE resources ADD COLUMN latest boolean;

ALTER TABLE orders ADD COLUMN version integer;
ALTER TABLE orders ADD COLUMN created_by varchar(255);
ALTER TABLE orders ADD COLUMN created_at timestamp with time zone;
ALTER TABLE orders ADD COLUMN deleted boolean;
ALTER TABLE orders ADD COLUMN latest boolean;

ALTER TABLE activities ADD COLUMN version integer;
ALTER TABLE activities ADD COLUMN created_by varchar(255);
ALTER TABLE activities ADD COLUMN created_at timestamp with time zone;
ALTER TABLE activities ADD COLUMN deleted boolean;
ALTER TABLE activities ADD COLUMN latest boolean;

-- set initial values for new columns
UPDATE resources SET version = 0 where version IS NULL;
UPDATE resources SET created_by = 'MIGRATION' where version IS NULL;
UPDATE resources SET created_at = CURRENT_TIMESTAMP where version IS NULL;
UPDATE resources SET deleted = false where version IS NULL;

UPDATE orders SET version = 0 where version IS NULL;
UPDATE orders SET created_by = 'MIGRATION' where version IS NULL;
UPDATE orders SET created_at = CURRENT_TIMESTAMP where version IS NULL;
UPDATE orders SET deleted = false where version IS NULL;

UPDATE activities SET version = 0 where version IS NULL;
UPDATE activities SET created_by = 'MIGRATION' where version IS NULL;
UPDATE activities SET created_at = CURRENT_TIMESTAMP where version IS NULL;
UPDATE activities SET deleted = false where version IS NULL;

-- make columns not null
ALTER TABLE resources ALTER COLUMN version SET NOT NULL;
ALTER TABLE resources ALTER COLUMN created_by SET NOT NULL;
ALTER TABLE resources ALTER COLUMN created_at SET NOT NULL;
ALTER TABLE resources ALTER COLUMN latest SET NOT NULL;
ALTER TABLE resources ALTER COLUMN deleted SET NOT NULL;

ALTER TABLE orders ALTER COLUMN version SET NOT NULL;
ALTER TABLE orders ALTER COLUMN created_by SET NOT NULL;
ALTER TABLE orders ALTER COLUMN created_at SET NOT NULL;
ALTER TABLE orders ALTER COLUMN latest SET NOT NULL;
ALTER TABLE orders ALTER COLUMN deleted SET NOT NULL;

ALTER TABLE activities ALTER COLUMN version SET NOT NULL;
ALTER TABLE activities ALTER COLUMN created_by SET NOT NULL;
ALTER TABLE activities ALTER COLUMN created_at SET NOT NULL;
ALTER TABLE activities ALTER COLUMN latest SET NOT NULL;
ALTER TABLE activities ALTER COLUMN deleted SET NOT NULL;

-- change primary key to id, version
ALTER TABLE resources DROP CONSTRAINT resources_pkey;
ALTER TABLE orders DROP CONSTRAINT orders_pkey;
ALTER TABLE activities DROP CONSTRAINT activities_pkey;

ALTER TABLE resources ADD CONSTRAINT resources_pkey PRIMARY KEY (id, version);
ALTER TABLE orders ADD CONSTRAINT orders_pkey PRIMARY KEY (id, version);
ALTER TABLE activities ADD CONSTRAINT activities_pkey PRIMARY KEY (id, version);

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.5.0',
  'strolch',
  'Added versioning to root elements',
  CURRENT_TIMESTAMP
);