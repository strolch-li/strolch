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

-- rename index ids_orders_date to idx_orders_date
DROP INDEX IF EXISTS ids_orders_date;
DROP INDEX IF EXISTS idx_orders_date;
CREATE INDEX idx_orders_date
    ON orders (date NULLS LAST)
;

-- add index to id column on table operations_log_values
CREATE INDEX idx_operations_log_values_id
    ON operations_log_values (id NULLS LAST)
;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.8',
  'strolch',
  'add index to id column on table operations_log_values and rename index ids_orders_date to idx_orders_date',
  CURRENT_TIMESTAMP
);
