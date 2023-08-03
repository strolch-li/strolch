
-- DB_VERSION
CREATE TABLE IF NOT EXISTS db_version (
  id serial primary key not null,
  app varchar(255) not null,
  version varchar(255) not null,
  description varchar(255) not null,
  created timestamp with time zone not null
);

-- RESOURCES
CREATE TABLE IF NOT EXISTS resources (
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

  PRIMARY KEY (type, id, version)
);

-- ORDERS
CREATE TYPE order_state AS ENUM ('CREATED', 'PLANNING', 'PLANNED', 'EXECUTION', 'STOPPED', 'WARNING', 'ERROR', 'EXECUTED', 'CLOSED');
CREATE TABLE IF NOT EXISTS orders (
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
  
  PRIMARY KEY (type, id, version)
);
DROP INDEX IF EXISTS ids_orders_date;
CREATE INDEX ids_orders_date
    ON orders (date NULLS LAST)
;

-- ACTIVITIES
CREATE TABLE IF NOT EXISTS activities (
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
  
  PRIMARY KEY (type, id, version)
);

-- AUDITS
CREATE TYPE access_type AS ENUM ('READ', 'CREATE', 'UPDATE', 'DELETE');
CREATE TABLE IF NOT EXISTS audits (
  id bigint PRIMARY KEY,
  username varchar(255) NOT NULL,
  firstname varchar(255) NOT NULL,
  lastname varchar(255) NOT NULL,
  date timestamp with time zone NOT NULL,

  element_type varchar(255) NOT NULL,
  element_sub_type varchar(255) NOT NULL,
  element_accessed varchar(255) NOT NULL,
  new_version timestamp with time zone,

  action varchar(255) NOT NULL,
  access_type access_type NOT NULL
);

-- Operations Log
CREATE TYPE log_severity_type AS ENUM ('Info', 'Notification', 'Warning', 'Error', 'Exception');
CREATE TYPE log_state_type AS ENUM ('Active', 'Inactive', 'Information');
CREATE TABLE IF NOT EXISTS operations_log (
  id varchar(255) PRIMARY KEY,
	realm varchar(255),
	dateTime timestamp with time zone,
	username varchar(255),
	severity log_severity_type,
	state log_state_type,
	locator varchar(1024),
	bundle varchar(255),
	key varchar(255),
	message text,
	stacktrace text
);
CREATE TABLE IF NOT EXISTS operations_log_values (
  id varchar(255),
  key varchar(255),
  value text
);

-- set version
INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.1.0',
  'strolch',
  'Initial schema version',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.2.0',
  'strolch',
  'Added new table for audits',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.2.1',
  'strolch',
  'Added new column app to table table version',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.3.0',
  'strolch',
  'Added new column element_sub_type to table audits',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.4.0',
  'strolch',
  'Added new table activities',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.5.0',
  'strolch',
  'Added versioning to root elements',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.5.1',
  'strolch',
  'Added state column to activity, and added new states',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.6.0',
  'strolch',
  'Added json column to all tables',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.7.0',
  'strolch',
  'Added persisting of operations log',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.8.0',
  'strolch',
  'Added updated_at column to all tables',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.0',
  'strolch',
  'Added log_state column to operations_log',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.1',
  'strolch',
  'Added bundle column to operations_log',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.2',
  'strolch',
  'create index on date orders table',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.3',
  'strolch',
  'add type column to primary key',
  CURRENT_TIMESTAMP
);
