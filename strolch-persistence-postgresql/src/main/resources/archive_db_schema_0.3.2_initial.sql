/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

  PRIMARY KEY (type, id, version)
);
DROP INDEX IF EXISTS ids_archive_orders_date;
CREATE INDEX ids_archive_orders_date
    ON archive_orders (date NULLS LAST)
;

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

  PRIMARY KEY (type, id, version)
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

  PRIMARY KEY (type, id, version)
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

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.3.1',
  'archive',
  'create index on date archive_orders table',
  CURRENT_TIMESTAMP
);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.3.2',
  'archive',
  'add type column to primary key',
  CURRENT_TIMESTAMP
);
