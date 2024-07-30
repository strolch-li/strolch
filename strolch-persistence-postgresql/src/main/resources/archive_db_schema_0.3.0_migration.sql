
-- RESOURCES
CREATE TABLE IF NOT EXISTS archive_resources (
  id varchar(255) not null,
  version integer not null,
  created_by varchar(255) not null,
  created_at timestamp with time zone not null,
  updated_at timestamp with time zone not null,
  deleted boolean not null,
  latest boolean not null,
  name varchar(255) not null,
  type varchar(255) not null,
  asxml xml,
  asjson json,

  PRIMARY KEY (id, version)
);

-- ACTIVITIES
CREATE TABLE IF NOT EXISTS archive_activities (
  id varchar(255) not null,
  version integer not null,
  created_by varchar(255) not null,
  created_at timestamp with time zone not null,
  updated_at timestamp with time zone not null,
  deleted boolean not null,
  latest boolean not null,
  name varchar(255) not null,
  type varchar(255) not null,
  state order_state,
  asxml xml,
  asjson json,

  PRIMARY KEY (id, version)
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.3.0',
  'archive',
  'Added resources and activities',
  CURRENT_TIMESTAMP
);
