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

-- add bundle column
ALTER TABLE operations_log ADD COLUMN bundle varchar(255);

-- set initial values for new columns
UPDATE operations_log SET bundle = '' where bundle IS NULL;

-- make columns not null
ALTER TABLE operations_log ALTER COLUMN bundle SET NOT NULL;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.1',
  'strolch',
  'Added bundle column to operations_log',
  CURRENT_TIMESTAMP
);
