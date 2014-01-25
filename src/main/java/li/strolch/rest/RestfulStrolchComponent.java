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
package li.strolch.rest;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RestfulStrolchComponent extends StrolchComponent {

	private static RestfulStrolchComponent instance;

	/**
	 * @param container
	 * @param componentName
	 */
	public RestfulStrolchComponent(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void start() {
		DBC.PRE.assertNull("Instance is already set! This component is a singleton resource!", instance);
		instance = this;

		super.start();
	}

	@Override
	public void stop() {
		instance = null;
		super.stop();
	}

	/**
	 * @return the RestfulStrolchComponent
	 */
	public static RestfulStrolchComponent getInstance() {
		DBC.PRE.assertNotNull("Not yet initialized!", instance);
		return instance;
	}

	@Override
	public ComponentContainer getContainer() {
		return super.getContainer();
	}

	public <T extends StrolchComponent> T getComponent(Class<T> clazz) {
		return getContainer().getComponent(clazz);
	}
}
