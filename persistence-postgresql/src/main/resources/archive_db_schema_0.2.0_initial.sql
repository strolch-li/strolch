
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
  asxml xml not null,

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
  'ipsc_archive',
  'Added updated_at column',
  CURRENT_TIMESTAMP
);
