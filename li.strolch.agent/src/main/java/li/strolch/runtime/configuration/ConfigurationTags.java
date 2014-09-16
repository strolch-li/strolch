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
package li.strolch.runtime.configuration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ConfigurationTags {

	public static final String ENV_GLOBAL = "global"; //$NON-NLS-1$

	public static final String STROLCH_CONFIGURATION_ENV_COMPONENT_PROPERTIES = "StrolchConfiguration/env/Component/Properties"; //$NON-NLS-1$
	public static final String STROLCH_CONFIGURATION_ENV_COMPONENT = "StrolchConfiguration/env/Component"; //$NON-NLS-1$
	public static final String STROLCH_CONFIGURATION_ENV_RUNTIME_PROPERTIES = "StrolchConfiguration/env/Runtime/Properties"; //$NON-NLS-1$
	public static final String STROLCH_CONFIGURATION_ENV_RUNTIME = "StrolchConfiguration/env/Runtime"; //$NON-NLS-1$
	public static final String STROLCH_CONFIGURATION_ENV = "StrolchConfiguration/env"; //$NON-NLS-1$

	public static final String APPLICATION_NAME = "applicationName"; //$NON-NLS-1$
	public static final String ID = "id"; //$NON-NLS-1$
	public static final String DEPENDS = "depends"; //$NON-NLS-1$
	public static final String IMPL = "impl"; //$NON-NLS-1$
	public static final String API = "api"; //$NON-NLS-1$
	public static final String NAME = "name"; //$NON-NLS-1$

}
