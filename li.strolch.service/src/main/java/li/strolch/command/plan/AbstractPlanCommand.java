/*
 * Copyright 2015 Martin Smock <martin.smock@bluewin.ch>
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
package li.strolch.command.plan;

import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.exception.StrolchException;
import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;

/**
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public abstract class AbstractPlanCommand extends Command {

	/**
	 * @param container
	 * @param tx
	 */
	public AbstractPlanCommand(final ComponentContainer container, final StrolchTransaction tx) {
		super(container, tx);
	}

	/**
	 * plan an {@link Action}.It iterates the {@link IValueChange} operators and registers the changes on the
	 * {@link StrolchTimedState} objects assigned to the {@link Resource} referenced by type and id.
	 * 
	 * @param action
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	protected void plan(final Action action) {

		final Locator locator = Locator.newBuilder(Tags.RESOURCE, action.getResourceType(), action.getResourceId())
				.build();
		final Resource resource = tx().findElement(locator);

		if (resource == null)
			throw new StrolchException("Resource with " + locator + " referenced by " + action.getLocator()
					+ " cannot be null!");

		tx().lock(resource);

		final List<IValueChange<? extends IValue<?>>> changes = action.getChanges();
		for (final IValueChange<?> change : changes) {
			final StrolchTimedState timedState = resource.getTimedState(change.getStateId());
			timedState.applyChange(change);
		}

		action.setState(State.PLANNED);
	}

	/**
	 * plan an {@link Activity} by navigating to the {#link Action} and delegating the planning depending on the
	 * {@link IActivityElement} class.
	 */
	protected void plan(final Activity activity) {
		
		// TODO Martin: Use a visitor pattern so we don't start with instanceof again...

		final Iterator<Entry<String, IActivityElement>> elementIterator = activity.elementIterator();

		while (elementIterator.hasNext()) {
			final IActivityElement activityElement = elementIterator.next().getValue();
			if (activityElement instanceof Activity)
				plan((Activity) activityElement);
			else if (activityElement instanceof Action)
				plan((Action) activityElement);
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	protected void unplan(final Action action) {

		final Locator locator = Locator.newBuilder(Tags.RESOURCE, action.getResourceType(), action.getResourceId())
				.build();
		final Resource resource = tx().findElement(locator);

		final List<IValueChange<? extends IValue<?>>> changes = action.getChanges();
		for (final IValueChange<?> change : changes) {
			final StrolchTimedState timedState = resource.getTimedState(change.getStateId());
			timedState.applyChange(change.getInverse());
		}

		action.setState(State.CREATED);
	}

	protected void unplan(final Activity activity) {

		final Iterator<Entry<String, IActivityElement>> elementIterator = activity.elementIterator();
		while (elementIterator.hasNext()) {

			final IActivityElement activityElement = elementIterator.next().getValue();

			if (activityElement instanceof Activity)
				unplan((Activity) activityElement);
			else if (activityElement instanceof Action)
				unplan((Action) activityElement);
		}
	}
}
