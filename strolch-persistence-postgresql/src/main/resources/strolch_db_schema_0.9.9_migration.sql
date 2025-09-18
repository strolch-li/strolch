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

-- add index to id column on table operations_log_values
DROP INDEX IF EXISTS idx_orders_type;
DROP INDEX IF EXISTS idx_orders_latest;
DROP INDEX IF EXISTS idx_resources_type;
DROP INDEX IF EXISTS idx_resources_latest;
DROP INDEX IF EXISTS idx_activities_type;
DROP INDEX IF EXISTS idx_activities_latest;

CREATE INDEX IF NOT EXISTS idx_orders_type ON orders (type);
CREATE INDEX IF NOT EXISTS idx_orders_latest ON orders (latest);
CREATE INDEX IF NOT EXISTS idx_resources_type ON resources (type);
CREATE INDEX IF NOT EXISTS idx_resources_latest ON resources (latest);
CREATE INDEX IF NOT EXISTS idx_activities_type ON activities (type);
CREATE INDEX IF NOT EXISTS idx_activities_latest ON activities (latest);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.9',
  'strolch',
  'add index to type and latest columns on all element tables',
  CURRENT_TIMESTAMP
);
