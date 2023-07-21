
-- update primary keys
alter table resources drop constraint resources_pkey;
alter table orders drop constraint orders_pkey;
alter table activities drop constraint activities_pkey;

alter table resources add constraint resources_pkey primary key (type, id, version);
alter table orders add constraint orders_pkey primary key (type, id, version);
alter table activities add constraint activities_pkey primary key (type, id, version);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.3',
  'strolch',
  'add type column to primary key',
  CURRENT_TIMESTAMP
);
