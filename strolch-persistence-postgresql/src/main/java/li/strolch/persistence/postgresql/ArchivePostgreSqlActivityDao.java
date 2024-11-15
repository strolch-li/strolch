/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.persistence.postgresql;

import li.strolch.persistence.api.TransactionResult;

import java.sql.Connection;

public class ArchivePostgreSqlActivityDao extends PostgreSqlActivityDao {

	public static final String TABLE_NAME = "archive_activities";

	public ArchivePostgreSqlActivityDao(DataType dataType, Connection connection, TransactionResult txResult,
			boolean versioningEnabled) {
		super(dataType, connection, txResult, versioningEnabled);
	}

	@Override
	protected String getTableName() {
		return TABLE_NAME;
	}
}
