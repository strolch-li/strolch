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
package li.strolch.persistence.api;

import java.sql.Connection;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface DataArchiveHandler {

	void run(StrolchTransaction tx, Consumer<ArchiveTransaction> runnable);

	<T> T runWithResult(StrolchTransaction tx, Function<ArchiveTransaction, T> runnable);

	ArchiveTransaction openArchiveTx(StrolchTransaction tx);

	OrderDao getOrderDao(ArchiveTransaction archiveTx);

	ResourceDao getResourceDao(ArchiveTransaction archiveTx);

	ActivityDao getActivityDao(ArchiveTransaction archiveTx);
}
