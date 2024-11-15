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

import li.strolch.persistence.api.ArchiveTransaction;
import li.strolch.persistence.api.StrolchTransaction;

import java.sql.Connection;

public class PostgreSqlArchiveTransaction extends ArchiveTransaction {

	private final Connection connection;

	protected PostgreSqlArchiveTransaction(StrolchTransaction tx, PostgreSqlDataArchiveHandler archiveHandler,
			Connection connection) {
		super(tx, archiveHandler);
		this.connection = connection;
	}

	protected Connection getConnection() {
		return this.connection;
	}

	@Override
	public void close() throws Exception {
		try {
			flush();
			this.connection.commit();
		} catch (Exception e) {
			this.connection.rollback();
			throw e;
		}
	}
}
