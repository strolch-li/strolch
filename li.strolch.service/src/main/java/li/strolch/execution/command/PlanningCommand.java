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
package li.strolch.execution.command;

import static li.strolch.utils.helper.StringHelper.DASH;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.util.Iterator;
import java.util.Map.Entry;

import li.strolch.exception.StrolchException;
import li.strolch.execution.policy.ConfirmationPolicy;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.visitor.IActivityElementVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.service.api.Command;

/**
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public abstract class PlanningCommand extends Command implements IActivityElementVisitor<Void> {

	public PlanningCommand(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void undo() {
		// do nothing
	}

	@Override
	public Void visitActivity(Activity activity) {
		if (activity.getState().compareTo(State.PLANNED) >= 0)
			return null;

		Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement element = iter.next().getValue();
			if (element.getState().compareTo(State.PLANNED) < 0)
				element.accept(this);
		}
		return null;
	}

	protected Resource getResource(Action action) {
		String resourceId = action.getResourceId();
		if (isEmpty(resourceId) || resourceId.equals(DASH))
			throw new StrolchException("No resourceId defined on action " + action.getLocator());
		String resourceType = action.getResourceType();
		if (isEmpty(resourceType) || resourceType.equals(DASH))
			throw new StrolchException("No resourceType defined on action " + action.getLocator());

		return tx().getResourceBy(resourceType, resourceId, true);
	}


	protected ConfirmationPolicy getConfirmationPolicy(Action action) {
		Resource resource = getResource(action);
		PolicyDef executionPolicyDef = resource.getPolicyDefs().getPolicyDef(ConfirmationPolicy.class.getSimpleName());
		return getComponent(PolicyHandler.class).getPolicy(executionPolicyDef, tx());
	}
}
