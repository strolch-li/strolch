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


DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'log_severity_type') THEN
        CREATE TYPE log_severity_type AS ENUM ('Info', 'Notification', 'Warning', 'Error', 'Exception');
    END IF;
END$$;


CREATE TABLE IF NOT EXISTS operations_log (
  id varchar(255) PRIMARY KEY,
	realm varchar(255),
	dateTime timestamp with time zone,
	username varchar(255),
	severity log_severity_type,
	locator varchar(1024),
	key varchar(255),
	message text,
	stacktrace text
);

CREATE TABLE IF NOT EXISTS operations_log_values (
  id varchar(255),
  key varchar(255),
  value text
);


INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.7.0',
  'strolch',
  'Added persisting of operations log',
  CURRENT_TIMESTAMP
);
