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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.SystemUserAction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.RunRunnable;

/**
 * <p>
 * A {@link StrolchComponent} is a configurable extension to Strolch. Every major feature should be implemented as a
 * {@link StrolchComponent} so that they can be easily added or removed from a Strolch runtime.
 * </p>
 * 
 * <p>
 * A {@link StrolchComponent} has access to the container and can perform different operations. They can be passive or
 * active and their life cycle is bound to the container's life cycle
 * </p>
 * 
 * <p>
 * A {@link StrolchComponent} is registered in the Strolch configuration file and can have different configuration
 * depending on the container's runtime environment
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchComponent {

	public static final String COMPONENT_VERSION_PROPERTIES = "/componentVersion.properties"; //$NON-NLS-1$
	protected static final Logger logger = LoggerFactory.getLogger(StrolchComponent.class);
	private final ComponentContainer container;
	private final String componentName;
	private ComponentState state;
	private ComponentVersion version;
	private ComponentConfiguration configuration;

	/**
	 * Constructor which takes a reference to the container and the component's name under which it can be retrieved at
	 * runtime (although one mostly retrieves the component by interface class for automatic casting)
	 * 
	 * @param container
	 *            the container
	 * @param componentName
	 *            the component name
	 */
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

	/**
	 * Returns the current component's state
	 * 
	 * @return the component's current state
	 */
	public ComponentState getState() {
		return this.state;
	}

	/**
	 * Returns the reference to the container for sub classes
	 * 
	 * @return the reference to the container
	 */
	protected ComponentContainer getContainer() {
		return this.container;
	}

	/**
	 * The components current configuration dependent on the environment which is loaded
	 * 
	 * @return the component's configuration
	 */
	protected ComponentConfiguration getConfiguration() {
		return this.configuration;
	}

	/**
	 * Can be used by sub classes to assert that the component is started and thus ready to use, before any component
	 * methods are used
	 */
	protected void assertStarted() {
		if (getState() != ComponentState.STARTED) {
			String msg = "Component {0} is not yet started!"; //$NON-NLS-1$
			throw new IllegalStateException(MessageFormat.format(msg, this.componentName));
		}
	}

	/**
	 * Can be used by sub classes to assert that the entire container is started and thus ready to use, before any
	 * component methods are used
	 */
	protected void assertContainerStarted() {
		if (this.container.getState() != ComponentState.STARTED) {
			String msg = "Container is not yet started!"; //$NON-NLS-1$
			throw new IllegalStateException(msg);
		}
	}

	/**
	 * Life cycle step setup. This is a very early step in the container's startup phase.
	 * 
	 * @param configuration
	 */
	public void setup(ComponentConfiguration configuration) {
		this.state = this.state.validateStateChange(ComponentState.SETUP);
	}

	/**
	 * Life cycle step initialize. Here you would typically read configuration values
	 * 
	 * @param configuration
	 * @throws Exception
	 */
	public void initialize(ComponentConfiguration configuration) throws Exception {
		this.configuration = configuration;
		this.state = this.state.validateStateChange(ComponentState.INITIALIZED);
	}

	/**
	 * Life cycle step start. This is the last step of startup and is where threads and connections etc. would be
	 * prepared. Can also be called after stop, to restart the component.
	 * 
	 * @throws Exception
	 */
	public void start() throws Exception {
		this.state = this.state.validateStateChange(ComponentState.STARTED);
	}

	/**
	 * Life cycle step stop. This is the first step in the tearing down of the container. Stop all active threads and
	 * connections here. After stop is called, another start might also be called to restart the component.
	 * 
	 * @throws Exception
	 */
	public void stop() throws Exception {
		this.state = this.state.validateStateChange(ComponentState.STOPPED);
	}

	/**
	 * Life cycle step destroy. This is the last step in the tearing down of the container. Here you would release
	 * remaining resources and the component can not be started anymore afterwards
	 * 
	 * @throws Exception
	 */
	public void destroy() throws Exception {
		this.state = this.state.validateStateChange(ComponentState.DESTROYED);
	}

	/**
	 * Performs the given {@link SystemUserAction} as a system user with the given username. Returns the action for
	 * chaining calls
	 * 
	 * @param username
	 *            the name of the system user to perform the action as
	 * @param action
	 *            the action to perform
	 * 
	 * @return the action performed for chaining calls
	 * 
	 * @throws PrivilegeException
	 */
	protected <V extends SystemUserAction> V runAs(String username, V action) throws PrivilegeException {
		return this.container.getPrivilegeHandler().runAsSystem(username, action);
	}

	/**
	 * Performs the given {@link SystemUserAction} as the privileged system user
	 * {@link StrolchConstants#PRIVILEGED_SYSTEM_USER}. Returns the action for chaining calls
	 * 
	 * @param action
	 *            the action to perform
	 * 
	 * @return the action performed for chaining calls
	 * 
	 * @throws PrivilegeException
	 */
	protected <V extends SystemUserAction> V runPrivileged(V action) throws PrivilegeException {
		return this.container.getPrivilegeHandler().runAsSystem(StrolchConstants.PRIVILEGED_SYSTEM_USER, action);
	}

	/**
	 * Performs the given {@link SystemUserAction} as the privileged system user
	 * {@link StrolchConstants#PRIVILEGED_SYSTEM_USER}. Returns the action for chaining calls
	 * 
	 * @param action
	 *            the action to perform
	 * 
	 * @throws PrivilegeException
	 */
	protected <T> T runPrivilegedRunnable(RunRunnable.Runnable<T> action) throws PrivilegeException {
		return this.container.getPrivilegeHandler()
				.runAsSystem(StrolchConstants.PRIVILEGED_SYSTEM_USER, new RunRunnable<>(action)).getResult();
	}

	/**
	 * Returns the version of this component. The version should be stored in the file
	 * {@link #COMPONENT_VERSION_PROPERTIES}. See {@link ComponentVersion} for more information
	 * 
	 * @return the component's version.
	 * @throws IOException
	 */
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
