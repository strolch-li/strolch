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
ALTER TYPE access_type ADD VALUE IF NOT EXISTS 'EXECUTE';
ALTER TABLE audits ADD COLUMN additional_data json;

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.9.5',
  'strolch',
  'extend audits with additional data and new access type',
  CURRENT_TIMESTAMP
);
