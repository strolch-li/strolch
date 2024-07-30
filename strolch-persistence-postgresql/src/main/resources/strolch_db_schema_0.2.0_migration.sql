
-- AUDITS
CREATE TYPE access_type AS ENUM ('READ', 'CREATE', 'UPDATE', 'DELETE');
CREATE TABLE IF NOT EXISTS audits (
  id bigint PRIMARY KEY,
  username VARCHAR(255) NOT NULL,
  firstname VARCHAR(255) NOT NULL,
  lastname VARCHAR(255) NOT NULL,
  date timestamp with time zone NOT NULL,

  element_type VARCHAR(255) NOT NULL,
  element_accessed VARCHAR(255) NOT NULL,
  new_version timestamp with time zone,

  action VARCHAR(255) NOT NULL,
  access_type access_type NOT NULL
);

-- set version
INSERT INTO db_version 
  (version, description, created) 
values(
  '0.2.0',
  'Added new table for audits',
  CURRENT_TIMESTAMP
);
