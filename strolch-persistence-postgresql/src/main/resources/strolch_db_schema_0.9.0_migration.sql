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


-- add new type
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'log_state_type') THEN
        CREATE TYPE log_state_type AS ENUM ('Active', 'Inactive', 'Information');
    END IF;
END$$;

-- add version columns
ALTER TABLE operations_log ADD COLUMN state log_state_type;

-- set initial values for new columns
UPDATE operations_log SET state = 'Information' where state IS NULL;

-- make columns not null
ALTER TABLE operations_log ALTER COLUMN state SET NOT NULL;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.0',
  'strolch',
  'Added log_state column to operations_log',
  CURRENT_TIMESTAMP
);