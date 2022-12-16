
-- ORDERS
CREATE TABLE IF NOT EXISTS archive_orders (
  id varchar(255) not null,
  version integer not null,
  created_by varchar(255) not null,
  created_at timestamp with time zone not null,
  updated_at timestamp with time zone not null,
  deleted boolean,
  latest boolean not null,
  name varchar(255),
  type varchar(255),
  state order_state,
  date timestamp with time zone,
  asxml xml,
  asjson json,

  PRIMARY KEY (id, version)
);

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
  '0.1.0',
  'archive',
  'Initial schema version',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.2.0',
  'archive',
  'Added updated_at column',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.3.0',
  'archive',
  'Added resources and activities',
  CURRENT_TIMESTAMP
);
