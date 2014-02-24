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
package li.strolch.agent.impl;

import java.text.MessageFormat;

import li.strolch.agent.api.StrolchRealm;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public enum DataStoreMode {
	EMPTY {
		@Override
		public StrolchRealm createRealm(String realm) {
			return new EmptyRealm(realm);
		}
	}, //
	TRANSIENT {
		@Override
		public StrolchRealm createRealm(String realm) {
			return new TransientRealm(realm);
		}
	}, //
	CACHED {
		@Override
		public StrolchRealm createRealm(String realm) {
			return new CachedRealm(realm);
		}
	}, //
	TRANSACTIONAL {
		@Override
		public StrolchRealm createRealm(String realm) {
			return new TransactionalRealm(realm);
		}
	}; //

	public abstract StrolchRealm createRealm(String realm);

	public static DataStoreMode parseDataStoreMode(String modeS) {
		for (DataStoreMode dataStoreMode : values()) {
			if (dataStoreMode.name().endsWith(modeS))
				return dataStoreMode;
		}

		throw new IllegalArgumentException(MessageFormat.format("There is no data store mode ''{0}''", modeS)); //$NON-NLS-1$
	}
}
