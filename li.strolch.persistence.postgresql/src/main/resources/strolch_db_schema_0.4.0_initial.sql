
-- DB_VERSION
CREATE TABLE IF NOT EXISTS db_version (
  id SERIAL PRIMARY KEY,
  app varchar(255),
  version varchar(255),
  description varchar(255),
  created timestamp with time zone
);

-- RESOURCES
CREATE TABLE IF NOT EXISTS resources (
  id varchar(255) PRIMARY KEY, 
  name VARCHAR(255),
  type VARCHAR(255),
  asxml xml
);

-- ORDERS
CREATE TYPE order_state AS ENUM ('CREATED', 'OPEN', 'EXECUTION', 'CLOSED');

CREATE TABLE IF NOT EXISTS orders (
  id varchar(255) PRIMARY KEY, 
  name VARCHAR(255),
  type VARCHAR(255),
  state order_state,
  date timestamp with time zone,
  asxml xml
);

-- ACTIVITIES
CREATE TABLE IF NOT EXISTS activities (
  id varchar(255) PRIMARY KEY, 
  name VARCHAR(255),
  type VARCHAR(255),
  asxml xml
);

-- AUDITS
CREATE TYPE access_type AS ENUM ('READ', 'CREATE', 'UPDATE', 'DELETE');
CREATE TABLE IF NOT EXISTS audits (
  id bigint PRIMARY KEY,
  username VARCHAR(255) NOT NULL,
  firstname VARCHAR(255) NOT NULL,
  lastname VARCHAR(255) NOT NULL,
  date timestamp with time zone NOT NULL,

  element_type VARCHAR(255) NOT NULL,
  element_sub_type VARCHAR(255) NOT NULL,
  element_accessed VARCHAR(255) NOT NULL,
  new_version timestamp with time zone,

  action VARCHAR(255) NOT NULL,
  access_type access_type NOT NULL
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
