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
import li.strolch.persistence.api.StrolchTransaction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Restrictable;

/**
 * <p>
 * Implementation of the Command Pattern to create re-usable components which are performed during
 * {@link StrolchTransaction StrolchTransactions} as part of the execution of {@link Service Services}
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
	 * Instantiate a new {@link Command}
	 * 
	 * @param container
	 *            the {@link ComponentContainer} to access components at runtime
	 * @param tx
	 */
	public Command(ComponentContainer container, StrolchTransaction tx) {
		this.container = container;
		this.tx = tx;
	}

	/**
	 * Allows the concrete {@link Command} implementation access to {@link StrolchComponent StrolchComponents} at
	 * runtime
	 * 
	 * @param clazz
	 *            the type of component to be returned
	 * 
	 * @return the component with the given {@link Class} which is registered on the {@link ComponentContainer}
	 * 
	 * @throws IllegalArgumentException
	 *             if the component with the given class does not exist
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
	 * transaction is committed. Add this {@link Command} to the transaction by calling
	 * {@link StrolchTransaction#addCommand(Command)}
	 * </p>
	 */
	public abstract void doCommand();

	/**
	 * <p>
	 * Should the transaction fail, either due to a {@link Command} throwing an exception when {@link #validate()} is
	 * called, or while committing the transaction, then this method should properly undo any changes it has done. It is
	 * imperative that this method does not throw further exceptions and that the state to be rolled back is remembered
	 * in the Command during committing
	 * </p>
	 */
	public abstract void undo();
}
