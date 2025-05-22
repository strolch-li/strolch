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

-- update primary keys
ALTER TABLE operations_log
    ALTER COLUMN severity TYPE VARCHAR(255);

DROP TYPE IF EXISTS log_severity_type;
CREATE TYPE log_severity_type AS ENUM ('Info', 'Notification', 'Warning', 'Error', 'Exception', 'System');

ALTER TABLE operations_log
    ALTER COLUMN severity TYPE log_severity_type
    USING (severity::log_severity_type);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.4',
  'strolch',
  'add log severity type value System',
  CURRENT_TIMESTAMP
);
