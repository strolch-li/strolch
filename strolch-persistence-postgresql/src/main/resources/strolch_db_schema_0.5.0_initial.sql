
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
  deleted boolean not null,
  latest boolean not null,
  name varchar(255) not null,
  type varchar(255) not null,
  asxml xml not null,
  
  PRIMARY KEY (id, version)
);

-- ORDERS
CREATE TYPE order_state AS ENUM ('CREATED', 'OPEN', 'EXECUTION', 'CLOSED');

CREATE TABLE IF NOT EXISTS orders (
  id varchar(255) not null,
  version integer not null,
  created_by varchar(255) not null,
  created_at timestamp with time zone not null,
  deleted boolean,
  latest boolean not null,
  name varchar(255),
  type varchar(255),
  state order_state,
  date timestamp with time zone,
  asxml xml not null,
  
  PRIMARY KEY (id, version)
);

-- ACTIVITIES
CREATE TABLE IF NOT EXISTS activities (
  id varchar(255) not null, 
  version integer not null,
  created_by varchar(255) not null,
  created_at timestamp with time zone not null,
  deleted boolean not null,
  latest boolean not null,
  name varchar(255) not null,
  type varchar(255) not null,
  asxml xml not null,
  
  PRIMARY KEY (id, version)
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
