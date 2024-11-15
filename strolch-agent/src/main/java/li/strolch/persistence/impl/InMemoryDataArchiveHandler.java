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

package li.strolch.persistence.impl;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.persistence.api.*;

import java.util.function.Consumer;
import java.util.function.Function;

public class InMemoryDataArchiveHandler extends StrolchComponent implements DataArchiveHandler {

	public InMemoryDataArchiveHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void run(StrolchTransaction tx, Consumer<ArchiveTransaction> runnable) {
		try (ArchiveTransaction archiveTx = openArchiveTx(tx)) {
			runnable.accept(archiveTx);
		} catch (Exception e) {
			throw new StrolchPersistenceException("InMemory archive action failed", e);
		}
	}

	@Override
	public <T> T runWithResult(StrolchTransaction tx, Function<ArchiveTransaction, T> runnable) {
		try (ArchiveTransaction archiveTx = openArchiveTx(tx)) {
			return runnable.apply(archiveTx);
		} catch (Exception e) {
			throw new StrolchPersistenceException("InMemory archive action failed", e);
		}
	}

	@Override
	public ArchiveTransaction openArchiveTx(StrolchTransaction tx) {
		return new InMemoryArchiveTransaction(tx, this);
	}

	@Override
	public OrderDao getOrderDao(ArchiveTransaction archiveTx) {
		return new InMemoryOrderDao();
	}

	@Override
	public ResourceDao getResourceDao(ArchiveTransaction archiveTx) {
		return new InMemoryResourceDao();
	}

	@Override
	public ActivityDao getActivityDao(ArchiveTransaction archiveTx) {
		return new InMemoryActivityDao();
	}
}
