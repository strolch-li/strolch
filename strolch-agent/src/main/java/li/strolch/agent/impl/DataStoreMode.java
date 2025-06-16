/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.agent.impl;

import li.strolch.agent.impl.eclipsestore.EclipseStoreRealm;

import java.text.MessageFormat;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public enum DataStoreMode {
	EMPTY {
		@Override
		public boolean isTransient() {
			return true;
		}

		@Override
		public boolean requiresPersistenceHandler() {
			return false;
		}

		@Override
		public InternalStrolchRealm createRealm(String realm) {
			return new EmptyRealm(realm);
		}
	},
	TRANSIENT {
		@Override
		public boolean isTransient() {
			return true;
		}

		@Override
		public boolean requiresPersistenceHandler() {
			return false;
		}

		@Override
		public InternalStrolchRealm createRealm(String realm) {
			return new TransientRealm(realm);
		}
	},
	CACHED {
		@Override
		public boolean isTransient() {
			return false;
		}

		@Override
		public boolean requiresPersistenceHandler() {
			return true;
		}

		@Override
		public InternalStrolchRealm createRealm(String realm) {
			return new CachedRealm(realm);
		}
	},
	ECLIPSE_STORE {
		@Override
		public boolean isTransient() {
			// data is persisted, but no persistence handler is required
			return false;
		}

		@Override
		public boolean requiresPersistenceHandler() {
			return false;
		}

		@Override
		public InternalStrolchRealm createRealm(String realm) {
			return new EclipseStoreRealm(realm);
		}
	};

	public abstract InternalStrolchRealm createRealm(String realm);

	/**
	 * Determines if the data store mode is transient, i.e. not data is persisted
	 *
	 * @return {@code true} if the data store mode is transient, otherwise {@code false}.
	 */
	public abstract boolean isTransient();

	/**
	 * Determines if the data store mode requires a {@link li.strolch.persistence.api.PersistenceHandler}
	 *
	 * @return {@code true} if the data store mode requires a {@link li.strolch.persistence.api.PersistenceHandler}
	 */
	public abstract boolean requiresPersistenceHandler();

	public static DataStoreMode parseDataStoreMode(String modeS) {
		for (DataStoreMode dataStoreMode : values()) {
			if (dataStoreMode.name().endsWith(modeS))
				return dataStoreMode;
		}

		throw new IllegalArgumentException(MessageFormat.format("There is no data store mode ''{0}''", modeS));
	}
}
