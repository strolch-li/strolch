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
package li.strolch.rest.inspector;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AgentRef {

	private static final AgentRef instance;

	static {
		instance = new AgentRef();
	}

	public static AgentRef getInstance() {
		return instance;
	}

	private StrolchAgent agent;

	private AgentRef() {
		// singleton
	}

	public void init(StrolchAgent agent) {
		DBC.PRE.assertNull("AgentRef has already been configured!", this.agent);
		this.agent = agent;
	}

	/**
	 * @return the agent
	 */
	public StrolchAgent getAgent() {
		DBC.PRE.assertNotNull("Not yet initialized!", agent);
		return this.agent;
	}

	public ComponentContainer getContainer() {
		return getAgent().getContainer();
	}

	public <T extends StrolchComponent> T getComponent(Class<T> clazz) {
		return getAgent().getContainer().getComponent(clazz);
	}
}
