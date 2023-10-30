/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.Order;
import li.strolch.model.activity.IActivityElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_ORDER;

/**
 * Interface for all Strolch policies, which are instantiated by the {@link PolicyHandler}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class StrolchPolicy {

	protected static final Logger logger = LoggerFactory.getLogger(StrolchPolicy.class);

	private final StrolchAgent agent;
	private final ComponentContainer container;
	private final StrolchTransaction tx;

	/**
	 * Instantiate a new {@link StrolchPolicy}
	 *
	 * @param tx
	 * 		the transaction for this policy
	 */
	public StrolchPolicy(StrolchTransaction tx) {
		this.agent = tx.getAgent();
		this.container = tx.getContainer();
		this.tx = tx;
	}

	/**
	 * Returns true if the given component is registered on th container
	 *
	 * @param clazz
	 * 		the type of component to check for
	 *
	 * @return true if the component is available
	 */
	public boolean hasComponent(Class<?> clazz) {
		return this.container.hasComponent(clazz);
	}

	/**
	 * Allows the concrete {@link Command} implementation access to {@link StrolchComponent StrolchComponents} at
	 * runtime
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
	 * @return the container
	 */
	protected StrolchAgent getAgent() {
		return this.agent;
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
	 * Returns true if this TX is still open, or committing, and thus can still be used
	 * @return true if this TX is still open, or committing, and thus can still be used
	 */
	protected boolean isTxOpen() {
		return this.tx.isOpen() || this.tx.isCommitting();
	}

	protected Order getOrder(IActivityElement element) {
		return tx().getOrderByRelation(element.getRootElement(), PARAM_ORDER, true);
	}

	/**
	 * @see Command#undo()
	 */
	public void undo() {
		// empty implementation
	}
}
