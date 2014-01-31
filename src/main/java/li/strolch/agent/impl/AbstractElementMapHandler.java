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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ElementMapHandler;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchException;
import li.strolch.runtime.StrolchConstants;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractElementMapHandler extends StrolchComponent implements ElementMapHandler {

	protected Map<String, StrolchRealm> realms;

	/**
	 * @param container
	 * @param componentName
	 */
	public AbstractElementMapHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public Set<String> getRealmNames() {
		return new HashSet<>(this.realms.keySet());
	}

	@Override
	public StrolchRealm getDefaultRealm() {
		return getRealm(StrolchConstants.DEFAULT_REALM);
	}

	@Override
	public StrolchRealm getRealm(String realm) {
		StrolchRealm strolchRealm = this.realms.get(realm);
		if (strolchRealm == null) {
			String msg = "No realm is configured with the name {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, realm);
			throw new StrolchException(msg);
		}
		return strolchRealm;
	}
}
