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

-- Convert to new type, casting via text representation
ALTER TABLE orders ADD COLUMN asjson json;
ALTER TABLE orders ALTER COLUMN asxml DROP NOT NULL;

ALTER TABLE resources ADD COLUMN asjson json;
ALTER TABLE resources ALTER COLUMN asxml DROP NOT NULL;

ALTER TABLE activities ADD COLUMN asjson json;
ALTER TABLE activities ALTER COLUMN asxml DROP NOT NULL;

INSERT INTO db_version 
  (version, app, description, created) 
values(
  '0.6.0',
  'strolch',
  'Added json column to all tables',
  CURRENT_TIMESTAMP
);
