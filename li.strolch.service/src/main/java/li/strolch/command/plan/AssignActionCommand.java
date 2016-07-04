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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

/**
 * Command to assign an {@link Action} to a new {@link Resource}.
 * 
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class AssignActionCommand extends PlanActionCommand {

	private String targetResourceType;
	private String targetResourceId;

	private String initialResourceType;
	private String initialResourceId;

	/**
	 * @param container
	 * @param tx
	 */
	public AssignActionCommand(final ComponentContainer container, final StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Action may not be null!", this.action);
		DBC.PRE.assertNotNull("Action attribute resourceId may not be null!", action.getResourceId());
		DBC.PRE.assertNotNull("Action attribute resourceType may not be null!", action.getResourceType());
		DBC.PRE.assertNotNull("Target resourceId may not be null!", targetResourceId);
		DBC.PRE.assertNotNull("Target resourceType may not be null!", targetResourceType);
	}

	@Override
	public void doCommand() {

		validate();

		// bookkeeping for undo
		initialResourceId = action.getResourceId();
		initialResourceType = action.getResourceType();

		// unplan the action
		unplan(action);

		// set target resource
		action.setResourceId(targetResourceId);
		action.setResourceType(targetResourceType);

		// finally plan the action to the assigned resource
		plan(action);
	}

	@Override
	public void undo() {

		validate();

		// unplan the action
		unplan(action);

		// set target resource
		action.setResourceId(initialResourceId);
		action.setResourceType(initialResourceType);

		// finally plan the action
		plan(action);
	}

	public void setTargetResourceType(String type) {
		this.targetResourceType = type;
	}

	public void setTargetResourceId(String id) {
		this.targetResourceId = id;
	}

}
