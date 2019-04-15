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
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;

import li.strolch.model.Locator;
import li.strolch.model.Tags;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.SystemAction;
import li.strolch.privilege.handler.SystemActionWithResult;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
	 * 		the container
	 * @param componentName
	 * 		the component name
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
	public ComponentContainer getContainer() {
		DBC.PRE.assertNotNull("container is null!", this.container);
		return this.container;
	}

	/**
	 * The components current configuration dependent on the environment which is loaded
	 *
	 * @return the component's configuration
	 */
	public ComponentConfiguration getConfiguration() {
		return this.configuration;
	}

	/**
	 * Return the {@link ExecutorService} instantiated for this agent
	 *
	 * @return the {@link ExecutorService} instantiated for this agent
	 */
	protected ExecutorService getExecutorService() {
		return this.container.getAgent().getExecutor();
	}

	/**
	 * Return the {@link ExecutorService} for the given poolName instantiated for this agent
	 *
	 * @param poolName
	 * 		the name of the pool
	 *
	 * @return the {@link ExecutorService} for the given poolName  instantiated for this agent
	 */
	protected ExecutorService getExecutorService(String poolName) {
		return this.container.getAgent().getExecutor(poolName);
	}

	/**
	 * Return the {@link ExecutorService} instantiated for this agent
	 *
	 * @return the {@link ExecutorService} instantiated for this agent
	 */
	protected ExecutorService getSingleThreadExecutor() {
		return this.container.getAgent().getSingleThreadExecutor();
	}

	/**
	 * Return the {@link ExecutorService} for the given poolName instantiated for this agent
	 *
	 * @param poolName
	 * 		the name of the pool
	 *
	 * @return the {@link ExecutorService} for the given poolName  instantiated for this agent
	 */
	protected ExecutorService getSingleThreadExecutor(String poolName) {
		return this.container.getAgent().getSingleThreadExecutor(poolName);
	}

	/**
	 * Return the {@link ScheduledExecutorService} instantiated for this agent
	 *
	 * @return the {@link ScheduledExecutorService} instantiated for this agent
	 */
	protected ScheduledExecutorService getScheduledExecutor() {
		return this.container.getAgent().getScheduledExecutor();
	}

	/**
	 * Return the {@link ScheduledExecutorService} for the given poolName instantiated for this agent
	 *
	 * @param poolName
	 * 		the name of the pool
	 *
	 * @return the {@link ScheduledExecutorService} instantiated for this agent
	 */
	protected ScheduledExecutorService getScheduledExecutor(String poolName) {
		return this.container.getAgent().getScheduledExecutor(poolName);
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
	 * 		the configuration
	 */
	public void setup(ComponentConfiguration configuration) {
		this.state = this.state.validateStateChange(ComponentState.SETUP, getName());
	}

	/**
	 * Life cycle step initialize. Here you would typically read configuration values
	 *
	 * @param configuration
	 * 		the configuration
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 */
	public void initialize(ComponentConfiguration configuration) throws Exception {
		this.configuration = configuration;
		this.state = this.state.validateStateChange(ComponentState.INITIALIZED, getName());
	}

	/**
	 * Life cycle step start. This is the last step of startup and is where threads and connections etc. would be
	 * prepared. Can also be called after stop, to restart the component.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 */
	public void start() throws Exception {
		this.state = this.state.validateStateChange(ComponentState.STARTED, getName());
	}

	/**
	 * Life cycle step stop. This is the first step in the tearing down of the container. Stop all active threads and
	 * connections here. After stop is called, another start might also be called to restart the component.
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 */
	public void stop() throws Exception {
		this.state = this.state.validateStateChange(ComponentState.STOPPED, getName());
	}

	/**
	 * Life cycle step destroy. This is the last step in the tearing down of the container. Here you would release
	 * remaining resources and the component can not be started anymore afterwards
	 *
	 * @throws Exception
	 * 		if something goes wrong
	 */
	public void destroy() throws Exception {
		this.state = this.state.validateStateChange(ComponentState.DESTROYED, getName());
	}

	/**
	 * Returns the reference to the {@link StrolchComponent} with the given name, if it exists. If it does not exist, an
	 * {@link IllegalArgumentException} is thrown
	 *
	 * @param clazz
	 * 		the type of component to return
	 *
	 * @return the component with the given name
	 *
	 * @throws IllegalArgumentException
	 * 		if the component does not exist
	 */
	protected <V> V getComponent(Class<V> clazz) {
		return this.container.getComponent(clazz);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the given system user
	 *
	 * @param username
	 * 		the name of the system user to perform the action as
	 * @param action
	 * 		the action to perform
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAs(String username, SystemAction action) throws PrivilegeException, Exception {
		this.container.getPrivilegeHandler().runAs(username, action);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the given system user
	 *
	 * @param username
	 * 		the name of the system user to perform the action as
	 * @param action
	 * 		the action to perform
	 *
	 * @return the result
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected <T> T runWithResult(String username, SystemActionWithResult<T> action)
			throws PrivilegeException, Exception {
		return this.container.getPrivilegeHandler().runWithResult(username, action);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the given system user
	 *
	 * @param username
	 * 		the name of the system user to perform the action as
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAs(String username, PrivilegedRunnable runnable) throws PrivilegeException, Exception {
		this.container.getPrivilegeHandler().runAs(username, runnable);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the given system user
	 *
	 * @param username
	 * 		the name of the system user to perform the action as
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @return the result
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected <T> T runWithResult(String username, PrivilegedRunnableWithResult<T> runnable)
			throws PrivilegeException, Exception {
		return this.container.getPrivilegeHandler().runWithResult(username, runnable);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the given system user
	 *
	 * @param action
	 * 		the action to perform
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAsAgent(SystemAction action) throws PrivilegeException, Exception {
		this.container.getPrivilegeHandler().runAsAgent(action);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the given system user
	 *
	 * @param action
	 * 		the action to perform
	 *
	 * @return the result
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected <T> T runAsAgentWithResult(SystemActionWithResult<T> action) throws PrivilegeException, Exception {
		return this.container.getPrivilegeHandler().runAsAgentWithResult(action);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the privileged system user {@link
	 * StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException, Exception {
		this.container.getPrivilegeHandler().runAsAgent(runnable);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the privileged system user {@link
	 * StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @return the result
	 *
	 * @throws PrivilegeException
	 * 		if the given username is not allowed to perform the action
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected <T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable)
			throws PrivilegeException, Exception {
		return this.container.getPrivilegeHandler().runAsAgentWithResult(runnable);
	}

	/**
	 * Opens a {@link StrolchTransaction} for the default realm and certificate
	 *
	 * @param cert
	 * 		the certificate authorizing the transaction
	 * @param readOnly
	 * 		if this TX is read-only
	 *
	 * @return the newly created transaction
	 */
	protected StrolchTransaction openTx(Certificate cert, boolean readOnly) {
		return getContainer().getRealm(cert).openTx(cert, this.getClass(), readOnly);
	}

	/**
	 * Opens a {@link StrolchTransaction} for the default realm and certificate
	 *
	 * @param cert
	 * 		the certificate authorizing the transaction
	 * @param action
	 * 		the action describing the transaction context
	 * @param readOnly
	 * 		if this TX is read-only
	 *
	 * @return the newly created transaction
	 */
	protected StrolchTransaction openTx(Certificate cert, String action, boolean readOnly) {
		return getContainer().getRealm(cert).openTx(cert, action, readOnly);
	}

	/**
	 * Opens a {@link StrolchTransaction} for the given realm and certificate
	 *
	 * @param realm
	 * 		the name of the realm in which to open the transaction
	 * @param cert
	 * 		the certificate authorizing the transaction
	 * @param readOnly
	 * 		if this TX is read-only
	 *
	 * @return the newly created transaction
	 */
	protected StrolchTransaction openTx(String realm, Certificate cert, boolean readOnly) {
		return getContainer().getRealm(realm).openTx(cert, this.getClass(), readOnly);
	}

	/**
	 * Opens a {@link StrolchTransaction} for the given realm and certificate
	 *
	 * @param realm
	 * 		the name of the realm in which to open the transaction
	 * @param cert
	 * 		the certificate authorizing the transaction
	 * @param clazz
	 * 		the clazz describing the transaction context
	 * @param readOnly
	 * 		if this TX is read-only
	 *
	 * @return the newly created transaction
	 */
	protected StrolchTransaction openTx(String realm, Certificate cert, Class<?> clazz, boolean readOnly) {
		return getContainer().getRealm(realm).openTx(cert, clazz, readOnly);
	}

	/**
	 * Opens a {@link StrolchTransaction} for the given realm and certificate
	 *
	 * @param realm
	 * 		the name of the realm in which to open the transaction
	 * @param cert
	 * 		the certificate authorizing the transaction
	 * @param action
	 * 		the action describing the transaction context
	 * @param readOnly
	 * 		if this TX is read-only
	 *
	 * @return the newly created transaction
	 */
	protected StrolchTransaction openTx(String realm, Certificate cert, String action, boolean readOnly) {
		return getContainer().getRealm(realm).openTx(cert, action, readOnly);
	}

	/**
	 * Returns the version of this component. The version should be stored in the file {@link
	 * #COMPONENT_VERSION_PROPERTIES}. See {@link ComponentVersion} for more information
	 *
	 * @return the component's version.
	 *
	 * @throws IOException
	 * 		if the properties file containing the version could not be read
	 */
	public ComponentVersion getVersion() throws IOException {
		if (this.version == null) {
			try (InputStream stream = getClass().getResourceAsStream(COMPONENT_VERSION_PROPERTIES)) {
				if (stream == null) {
					throw new RuntimeException("/componentVersion.properties does not exist"); //$NON-NLS-1$
				}
				Properties properties = new Properties();
				properties.load(stream);

				this.version = new ComponentVersion(getName(), properties);
			}
		}

		return this.version;
	}

	/**
	 * @return Returns the locator of this agent
	 */
	public Locator getLocator() {
		return Locator.valueOf(Tags.AGENT, getName());
	}
}
