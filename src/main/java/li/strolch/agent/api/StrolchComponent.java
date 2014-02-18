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
package li.strolch.agent.api;

import java.io.IOException;
import java.io.InputStream;
import java.text.MessageFormat;
import java.util.Properties;

import li.strolch.runtime.configuration.ComponentConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class StrolchComponent {

	private static final String COMPONENT_VERSION_PROPERTIES = "/componentVersion.properties"; //$NON-NLS-1$
	protected static final Logger logger = LoggerFactory.getLogger(StrolchComponent.class);
	private final ComponentContainer container;
	private final String componentName;
	private ComponentState state;
	private ComponentVersion version;

	public StrolchComponent(ComponentContainer container, String componentName) {
		this.container = container;
		this.componentName = componentName;
		this.state = ComponentState.UNDEFINED;
	}

	/**
	 * @return the componentName
	 */
	public String getName() {
		return this.componentName;
	}

	public ComponentState getState() {
		return this.state;
	}

	protected ComponentContainer getContainer() {
		return this.container;
	}

	protected void assertStarted() {
		if (getState() != ComponentState.STARTED) {
			String msg = "Component {0} is not yet started!"; //$NON-NLS-1$
			throw new IllegalStateException(MessageFormat.format(msg, this.componentName));
		}
	}

	protected void assertContainerStarted() {
		if (this.container.getState() != ComponentState.STARTED) {
			String msg = "Container is not yet started!"; //$NON-NLS-1$
			throw new IllegalStateException(msg);
		}
	}

	public void setup(ComponentConfiguration configuration) {
		this.state = this.state.validateStateChange(ComponentState.SETUP);
	}

	public void initialize(ComponentConfiguration configuration) {
		this.state = this.state.validateStateChange(ComponentState.INITIALIZED);
	}

	public void start() {
		this.state = this.state.validateStateChange(ComponentState.STARTED);
	}

	public void stop() {
		this.state = this.state.validateStateChange(ComponentState.STOPPED);
	}

	public void destroy() {
		this.state = this.state.validateStateChange(ComponentState.DESTROYED);
	}

	public ComponentVersion getVersion() throws IOException {
		if (this.version == null) {
			try (InputStream stream = getClass().getResourceAsStream(COMPONENT_VERSION_PROPERTIES)) {
				if (stream == null) {
					throw new RuntimeException("/componentVersion.properties does not exist"); //$NON-NLS-1$
				}
				Properties properties = new Properties();
				properties.load(stream);

				ComponentVersion componentVersion = new ComponentVersion(getName(), properties);
				this.version = componentVersion;
			}
		}

		return this.version;
	}
}
