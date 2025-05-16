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
alter table archive_resources drop constraint archive_resources_pkey;
alter table archive_orders drop constraint archive_orders_pkey;
alter table archive_activities drop constraint archive_activities_pkey;

alter table archive_resources add constraint archive_resources_pkey primary key (type, id, version);
alter table archive_orders add constraint archive_orders_pkey primary key (type, id, version);
alter table archive_activities add constraint archive_activities_pkey primary key (type, id, version);

INSERT INTO db_version
  (version, app, description, created)
values(
  '0.3.2',
  'archive',
  'add type column to primary key',
  CURRENT_TIMESTAMP
);
