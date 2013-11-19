/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.agent;

import java.text.MessageFormat;

import li.strolch.runtime.configuration.ComponentConfiguration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public enum DataStoreMode {
	EMPTY {
		@Override
		public AgentLifecycleController getAgentLifecycleController(StrolchAgent strolchAgent,
				ComponentConfiguration configuration) {
			return new EmptyDataStoreModeAgentInitializer(strolchAgent, configuration);
		}
	}, //
	TRANSIENT {
		@Override
		public AgentLifecycleController getAgentLifecycleController(StrolchAgent strolchAgent,
				ComponentConfiguration configuration) {
			return new TransientDataStoreModeAgentInitializer(strolchAgent, configuration);
		}
	}, //
	CACHED {
		@Override
		public AgentLifecycleController getAgentLifecycleController(StrolchAgent strolchAgent,
				ComponentConfiguration configuration) {
			throw new UnsupportedOperationException(MessageFormat.format("The mode {0} is not yet supported!", this)); //$NON-NLS-1$
		}
	}, //
	TRANSACTIONAL {
		@Override
		public AgentLifecycleController getAgentLifecycleController(StrolchAgent strolchAgent,
				ComponentConfiguration configuration) {
			throw new UnsupportedOperationException(MessageFormat.format("The mode {0} is not yet supported!", this)); //$NON-NLS-1$
		}
	}; //

	public AgentLifecycleController getAgentLifecycleController(StrolchAgent strolchAgent,
			ComponentConfiguration configuration) {
		throw new UnsupportedOperationException("Please implement in enum!"); //$NON-NLS-1$
	}

	public static DataStoreMode parseDataStoreMode(String modeS) {
		for (DataStoreMode dataStoreMode : values()) {
			if (dataStoreMode.name().endsWith(modeS))
				return dataStoreMode;
		}

		throw new IllegalArgumentException(MessageFormat.format("There is no data store mode ''{0}''", modeS)); //$NON-NLS-1$
	}
}
