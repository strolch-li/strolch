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
