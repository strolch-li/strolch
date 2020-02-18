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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.PolicyContainer;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.policy.StrolchPolicy;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.SystemAction;
import li.strolch.privilege.handler.SystemActionWithResult;
import li.strolch.privilege.model.Restrictable;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Implementation of the Command Pattern to create re-usable components which are performed during {@link
 * StrolchTransaction StrolchTransactions} as part of the execution of {@link Service Services}
 * </p>
 *
 * <p>
 * <b>Note:</b> Do not call {@link #doCommand()} from {@link Service Services} or other places. Add {@link Command}
 * instances to a {@link StrolchTransaction} by calling {@link StrolchTransaction#addCommand(Command)}
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class Command implements Restrictable {

	protected static final Logger logger = LoggerFactory.getLogger(Command.class);
	private final ComponentContainer container;
	private final StrolchTransaction tx;

	/**
	 * Instantiate a new command
	 *
	 * @param tx
	 * 		the transaction
	 */
	public Command(StrolchTransaction tx) {
		this.container = tx.getContainer();
		this.tx = tx;
	}

	/**
	 * Instantiate a new command
	 *
	 * @param container
	 * 		the {@link ComponentContainer} to access components at runtime
	 * @param tx
	 * 		the transaction
	 */
	public Command(ComponentContainer container, StrolchTransaction tx) {
		this.container = container;
		this.tx = tx;
	}

	/**
	 * Allows the concrete command implementation access to {@link StrolchComponent StrolchComponents} at runtime
	 *
	 * @param clazz
	 * 		the type of component to be returned
	 *
	 * @return the component with the given {@link Class} which is registered on the {@link ComponentContainer}
	 *
	 * @throws IllegalArgumentException
	 * 		if the component with the given class does not exist
	 */
	protected <V> V getComponent(Class<V> clazz) throws IllegalArgumentException {
		return this.container.getComponent(clazz);
	}

	/**
	 * @return the container
	 */
	protected ComponentContainer getContainer() {
		return this.container;
	}

	/**
	 * Returns a {@link StrolchPolicy} instance from the given parameters
	 *
	 * @param policyClass
	 * 		the policy type to return. The simple name of the class determines the type of Policy to return.
	 * @param policyContainer
	 * 		the container
	 *
	 * @return the policy
	 */
	protected <T extends StrolchPolicy> T getPolicy(Class<T> policyClass, PolicyContainer policyContainer) {
		PolicyDefs policyDefs = policyContainer.getPolicyDefs();
		PolicyDef policyDef = policyDefs.getPolicyDef(policyClass.getSimpleName());
		PolicyHandler policyHandler = getComponent(PolicyHandler.class);
		@SuppressWarnings("unchecked")
		T policy = (T) policyHandler.getPolicy(policyDef, tx());
		return policy;
	}

	/**
	 * Performs the given {@link SystemAction} as a system user with the given username
	 *
	 * @param username
	 * 		the name of the system user to perform the action as
	 * @param action
	 * 		the action to perform
	 *
	 * @throws PrivilegeException
	 * 		if there is something wrong
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAs(String username, SystemAction action) throws PrivilegeException, Exception {
		this.container.getPrivilegeHandler().runAs(username, action);
	}

	/**
	 * Performs the given {@link SystemAction} as a system user with the given username
	 *
	 * @param username
	 * 		the name of the system user to perform the action as
	 * @param action
	 * 		the action to perform
	 *
	 * @return the result
	 *
	 * @throws PrivilegeException
	 * 		if there is something wrong
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected <T> T runWithResult(String username, SystemActionWithResult<T> action)
			throws PrivilegeException, Exception {
		return this.container.getPrivilegeHandler().runWithResult(username, action);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as a system user with the given username
	 *
	 * @param username
	 * 		the name of the system user to perform the action as
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @throws PrivilegeException
	 * 		if there is something wrong
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAs(String username, PrivilegedRunnable runnable) throws PrivilegeException, Exception {
		this.container.getPrivilegeHandler().runAs(username, runnable);
	}

	/**
	 * Performs the given {@link PrivilegedRunnableWithResult} as a system user with the given username
	 *
	 * @param username
	 * 		the name of the system user to perform the action as
	 * @param runnable
	 * 		the runnable to perform
	 *
	 * @return the result
	 *
	 * @throws PrivilegeException
	 * 		if there is something wrong
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected <V> V runWithResult(String username, PrivilegedRunnableWithResult<V> runnable)
			throws PrivilegeException, Exception {
		return this.container.getPrivilegeHandler().runWithResult(username, runnable);
	}

	/**
	 * Performs the given {@link SystemAction} as the privileged system user {@link StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param action
	 * 		the action to perform
	 *
	 * @throws PrivilegeException
	 * 		if there is something wrong
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAsAgent(SystemAction action) throws PrivilegeException, Exception {
		this.container.getPrivilegeHandler().runAsAgent(action);
	}

	/**
	 * Performs the given {@link SystemAction} as the privileged system user {@link StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param action
	 * 		the action to perform
	 *
	 * @return the result
	 *
	 * @throws PrivilegeException
	 * 		if there is something wrong
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected <V> V runAsAgentWithResult(SystemActionWithResult<V> action) throws PrivilegeException, Exception {
		return this.container.getPrivilegeHandler().runAsAgentWithResult(action);
	}

	/**
	 * Performs the given {@link PrivilegedRunnable} as the privileged system user {@link
	 * StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable
	 * 		the action to perform
	 *
	 * @throws PrivilegeException
	 * 		if there is something wrong
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected void runAsAgent(PrivilegedRunnable runnable) throws PrivilegeException, Exception {
		this.container.getPrivilegeHandler().runAsAgent(runnable);
	}

	/**
	 * Performs the given {@link PrivilegedRunnableWithResult} as the privileged system user {@link
	 * StrolchConstants#SYSTEM_USER_AGENT}
	 *
	 * @param runnable
	 * 		the action to perform
	 *
	 * @return the result
	 *
	 * @throws PrivilegeException
	 * 		if there is something wrong
	 * @throws Exception
	 * 		if anything else goes wrong during execution
	 */
	protected <V> V runAsAgentWithResult(PrivilegedRunnableWithResult<V> runnable)
			throws PrivilegeException, Exception {
		return this.container.getPrivilegeHandler().runAsAgentWithResult(runnable);
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
	 * @see li.strolch.privilege.model.Restrictable#getPrivilegeName()
	 */
	@Override
	public String getPrivilegeName() {
		return Command.class.getName();
	}

	/**
	 * @see li.strolch.privilege.model.Restrictable#getPrivilegeValue()
	 */
	@Override
	public Object getPrivilegeValue() {
		return this.getClass().getName();
	}

	/**
	 * To ensure that as few possibilities for exceptions as possible occur when {@link #doCommand()} is called, the
	 * {@link Command} should verify any input data so that a {@link #doCommand()} can be performed without exceptions
	 */
	public abstract void validate();

	/**
	 * <p>
	 * Clients implement this method to perform the work which is to be done in this {@link Command}.
	 * </p>
	 *
	 * <p>
	 * <b>Note:</b> Do not call this method directly, this method is called by the {@link StrolchTransaction} when the
	 * transaction is committed. Add this {@link Command} to the transaction by calling {@link
	 * StrolchTransaction#addCommand(Command)}
	 * </p>
	 */
	public abstract void doCommand();

	/**
	 * <p>
	 * This method can be used to undo actions peformed during the command, should the TX fail. In earlier versions of
	 * Strolch this was important to undo model changes, but the model changes are only visible after a commit succeeds,
	 * so this is no longer necessary.
	 * </p>
	 */
	public void undo() {
		// do nothing
	}
}
