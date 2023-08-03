
-- update primary keys
alter table archive_resources drop constraint archive_resources_pkey;
alter table archive_orders drop constraint archive_orders_pkey;
alter table archive_activities drop constraint archive_activities_pkey;

alter table archive_resources add constraint archive_resources_pkey primary key (type, id, version);
alter table archive_orders add constraint archive_orders_pkey primary key (type, id, version);
alter table archive_activities add constraint archive_activities_pkey primary key (type, id, version);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.3.2',
  'archive',
  'add type column to primary key',
  CURRENT_TIMESTAMP
);
