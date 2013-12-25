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
package li.strolch.runtime.agent;

import java.text.MessageFormat;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public enum DataStoreMode {
	EMPTY {
		@Override
		public ElementMapHandlerConfigurator getElementMapConfigurationConfigurator() {
			return new EmptyElementMapHandlerConfigurator();
		}
	}, //
	TRANSIENT {
		@Override
		public ElementMapHandlerConfigurator getElementMapConfigurationConfigurator() {
			return new TransientElementMapHandlerConfigurator();
		}
	}, //
	CACHED {
		@Override
		public ElementMapHandlerConfigurator getElementMapConfigurationConfigurator() {
			return new CachedElementMapHandlerConfigurator();
		}
	}, //
	TRANSACTIONAL {
		@Override
		public ElementMapHandlerConfigurator getElementMapConfigurationConfigurator() {
			return new TransactionalElementMapHandlerConfigurator();
		}
	}; //

	public abstract ElementMapHandlerConfigurator getElementMapConfigurationConfigurator();

	public static DataStoreMode parseDataStoreMode(String modeS) {
		for (DataStoreMode dataStoreMode : values()) {
			if (dataStoreMode.name().endsWith(modeS))
				return dataStoreMode;
		}

		throw new IllegalArgumentException(MessageFormat.format("There is no data store mode ''{0}''", modeS)); //$NON-NLS-1$
	}
}
