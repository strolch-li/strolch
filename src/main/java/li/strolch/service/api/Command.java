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
package li.strolch.service.api;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.agent.api.OrderMap;
import li.strolch.runtime.agent.api.ResourceMap;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import ch.eitchnet.privilege.model.Restrictable;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class Command implements Restrictable {

	private final DefaultServiceHandler serviceHandler;
	private final StrolchTransaction tx;

	public Command(DefaultServiceHandler serviceHandler, StrolchTransaction tx) {
		this.serviceHandler = serviceHandler;
		this.tx = tx;
	}

	public <V> V getComponent(Class<V> clazz) {
		return this.serviceHandler.getComponent(clazz);
	}

	public RuntimeConfiguration getRuntimeConfiguration() {
		return this.serviceHandler.getRuntimeConfiguration();
	}

	public ResourceMap getResourceMap(String realm) {
		return this.serviceHandler.getResourceMap(realm);
	}

	public OrderMap getOrderMap(String realm) {
		return this.serviceHandler.getOrderMap(realm);
	}

	/**
	 * Returns the {@link StrolchTransaction} bound to this {@link Command}'s runtime
	 * 
	 * @return the {@link StrolchTransaction} bound to this {@link Command}'s runtime
	 */
	protected StrolchTransaction tx() {
		return this.tx;
	}

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeName()
	 */
	@Override
	public String getPrivilegeName() {
		return Command.class.getName();
	}

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeValue()
	 */
	@Override
	public Object getPrivilegeValue() {
		return this.getClass().getName();
	}

	public abstract void doCommand();
}
