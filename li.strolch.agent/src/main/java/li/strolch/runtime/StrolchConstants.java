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
package li.strolch.runtime;

import static ch.eitchnet.utils.helper.StringHelper.DOT;
import li.strolch.agent.api.ObserverHandler;
import li.strolch.model.StrolchModelConstants;
import li.strolch.persistence.api.PersistenceHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class StrolchConstants {

	public static final String ENV_STROLCH = "ENV_STROLCH";
	public static final String PERSISTENCE_HANDLER = PersistenceHandler.class.getSimpleName();
	public static final String OBSERVER_HANDLER = ObserverHandler.class.getSimpleName();
	public static final String PRIVILEGE_HANDLER = "PrivilegeHandler";

	public static final String DEFAULT_REALM = "defaultRealm";

	public static final String DEFAULT_XML_VERSION = "1.0"; //$NON-NLS-1$
	public static final String DEFAULT_ENCODING = "utf-8"; //$NON-NLS-1$

	/**
	 * @see StrolchModelConstants#TEMPLATE
	 */
	public static final String TEMPLATE = StrolchModelConstants.TEMPLATE;

	/**
	 * @see StrolchModelConstants#INTERPRETATION_RESOURCE_REF
	 */
	public static final String INTERPRETATION_RESOURCE_REF = StrolchModelConstants.INTERPRETATION_RESOURCE_REF;

	/**
	 * @see StrolchModelConstants#INTERPRETATION_ORDER_REF
	 */
	public static final String INTERPRETATION_ORDER_REF = StrolchModelConstants.INTERPRETATION_ORDER_REF;

	public static String makeRealmKey(String realmName, String key) {
		String realmKey = key;
		if (!realmName.equals(DEFAULT_REALM))
			realmKey += DOT + realmName;
		return realmKey;
	}
}
