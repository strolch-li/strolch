
CREATE TABLE IF NOT EXISTS db_version (
  id SERIAL PRIMARY KEY,
  version varchar(255),
  description varchar(255),
  created timestamp with time zone
);

CREATE TABLE IF NOT EXISTS resources (
  id varchar(255) PRIMARY KEY, 
  name VARCHAR(255),
  type VARCHAR(255),
  asxml xml
);

CREATE TYPE order_state AS ENUM ('CREATED', 'OPEN', 'EXECUTION', 'CLOSED');

CREATE TABLE IF NOT EXISTS orders (
  id varchar(255) PRIMARY KEY, 
  name VARCHAR(255),
  type VARCHAR(255),
  state order_state,
  date timestamp with time zone,
  asxml xml
);

INSERT INTO db_version 
  (version, description, created) 
values(
  '0.1.0',
  'Initial schema version',
  CURRENT_TIMESTAMP
);
