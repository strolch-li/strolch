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

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.runtime.observer.ObserverHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class StrolchConstants {

	public static final String PERSISTENCE_HANDLER = PersistenceHandler.class.getSimpleName();
	public static final String OBSERVER_HANDLER = ObserverHandler.class.getSimpleName();
	public static final String PRIVILEGE_HANDLER = "PrivilegeHandler";

	public static final String DEFAULT_REALM = "defaultRealm";

	public static final String DEFAULT_XML_VERSION = "1.0"; //$NON-NLS-1$
	public static final String DEFAULT_ENCODING = "utf-8"; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as a reference
	 * to a {@link Resource}
	 */
	public static final String INTERPRETATION_RESOURCE_REF = "Resource-Ref"; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as a reference
	 * to an {@link Order}
	 */
	public static final String INTERPRETATION_ORDER_REF = "Order-Ref"; //$NON-NLS-1$
}
