/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.db;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class DbConstants {

	public static final String PROP_USE_ENV = "db.useEnv";
	public static final String PROP_DB_URL = "db.url";
	public static final String PROP_DB_IGNORE_REALM = "db.ignore.realm";
	public static final String PROP_DB_USERNAME = "db.username";
	public static final String PROP_DB_PASSWORD = "db.password";
	public static final String PROP_DB_VERBOSE = "db.verbose";
	public static final String PROP_DB_ALLOW_HOST_OVERRIDE_ENV = "db.allowHostOverrideEnv";
	public static final String PROP_DB_HOST_OVERRIDE = "db.hostOverride";
	public static final String PROP_ALLOW_SCHEMA_CREATION = "allowSchemaCreation";
	public static final String PROP_ALLOW_SCHEMA_MIGRATION = "allowSchemaMigration";
	public static final String PROP_ALLOW_SCHEMA_DROP = "allowSchemaDrop";
	public static final String PROP_ALLOW_DATA_INIT_ON_SCHEMA_CREATE = "allowDataInitOnSchemaCreate";
	public static final String PROP_ALLOW_DATA_INIT_ON_EMPTY_DB = "allowDataInitOnEmptyDb";
	public static final String PROP_DB_VERSION = "db_version";
	public static final String RESOURCE_DB_VERSION = "/{0}_db_version.properties";

	public static final String ENV_DB_HOST_OVERRIDE = "DB_HOST_OVERRIDE";
}
