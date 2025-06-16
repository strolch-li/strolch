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

CREATE TYPE order_state1 AS ENUM ('CREATED', 'PLANNING', 'PLANNED', 'EXECUTION', 'STOPPED', 'WARNING', 'ERROR', 'EXECUTED', 'CLOSED');

-- Convert to new type, casting via text representation
ALTER TABLE orders 
  ALTER COLUMN state TYPE order_state1 
    USING (state::text::order_state1);

DROP TYPE order_state;
ALTER TYPE order_state1 RENAME TO order_state;

-- add state columns
ALTER TABLE activities ADD COLUMN state order_state;

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.5.1',
  'strolch',
  'Added state column to activity, and added new states',
  CURRENT_TIMESTAMP
);
