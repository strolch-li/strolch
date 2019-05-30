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

import static li.strolch.utils.helper.StringHelper.DASH;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchException;
import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.helper.StringHelper;

/**
 * Interface for all Strolch policies, which are instantiated by the {@link PolicyHandler}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class StrolchPolicy {

	protected static final Logger logger = LoggerFactory.getLogger(StrolchPolicy.class);

	private final ComponentContainer container;
	private final StrolchTransaction tx;

	/**
	 * Instantiate a new {@link StrolchPolicy}
	 *
	 * @param container
	 * 		the {@link ComponentContainer} to access components at runtime
	 * @param tx
	 * 		the transaction for this policy
	 */
	public StrolchPolicy(ComponentContainer container, StrolchTransaction tx) {
		this.container = container;
		this.tx = tx;
	}

	/**
	 * Instantiate a new {@link StrolchPolicy}
	 *
	 * @param tx
	 * 		the transaction for this policy
	 */
	public StrolchPolicy(StrolchTransaction tx) {
		this.container = tx.getContainer();
		this.tx = tx;
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
	 * Returns the {@link StrolchTransaction} bound to this {@link Command}'s runtime
	 *
	 * @return the {@link StrolchTransaction} bound to this {@link Command}'s runtime
	 */
	protected StrolchTransaction tx() {
		return this.tx;
	}

	/**
	 * Return the defined {@link Resource} on the given {@link Action}
	 *
	 * @param action
	 * 		the action for which to get the {@link Resource}
	 *
	 * @return the {@link Resource}
	 *
	 * @throws IllegalArgumentException
	 * 		if the resource is not defined on the action, i.e. fields are empty or a dash
	 * @throws StrolchException
	 * 		if the resource does not exist, which is referenced by the action
	 */
	protected Resource getResource(Action action) throws IllegalArgumentException, StrolchException {
		String resourceId = action.getResourceId();
		if (StringHelper.isEmpty(resourceId) || resourceId.equals(DASH))
			throw new IllegalArgumentException("No resourceId defined on action " + action.getLocator());
		String resourceType = action.getResourceType();
		if (StringHelper.isEmpty(resourceType) || resourceType.equals(DASH))
			throw new IllegalArgumentException("No resourceType defined on action " + action.getLocator());

		return this.tx.getResourceBy(resourceType, resourceId, true);
	}

	/**
	 * @see Command#undo()
	 */
	public abstract void undo();
}
