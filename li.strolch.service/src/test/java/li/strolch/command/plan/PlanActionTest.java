/*
 * Copyright 2014 Martin Smock <smock.martin@gmail.com>
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

import static li.strolch.model.ModelGenerator.STATE_INTEGER_ID;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_NAME;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_TIME_20;

import java.util.SortedSet;

import li.strolch.model.ModelGenerator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.ActionState;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.ValueChange;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class PlanActionTest {

	private Resource resource;

	@Before
	public void init() {

		resource = ModelGenerator.createResource("@1", "Test With States", "Stated");

		// add a integer state
		final IntegerTimedState timedState = new IntegerTimedState(STATE_INTEGER_ID, STATE_INTEGER_NAME);
		timedState.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)));
		resource.addTimedState(timedState);
	}

	@Test
	public void test() {

		final Action action = new Action("action_1", "Action 1", "Use");
		action.setStart(STATE_TIME_10);
		action.setEnd(STATE_TIME_20);
		
		Assert.assertEquals(ActionState.CREATED, action.getState()); 

		final IntegerParameter iP = new IntegerParameter("quantity", "Occupation", 1);
		action.addParameterBag(new ParameterBag("objective", "Objective", "Don't know"));
		action.addParameter("objective", iP);

		createChanges(action);

		final PlanActionCommand cmd = new PlanActionCommand(null, null);
		cmd.setAction(action);
		cmd.setResource(resource);
		cmd.doCommand();
		
		// check the state 
		Assert.assertEquals(ActionState.PLANNED, action.getState()); 

		// check if we get the expected result
		StrolchTimedState<IValue<Integer>> timedState = resource.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> timeEvolution = timedState.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> values = timeEvolution.getValues();

		Assert.assertEquals(3, values.size());

		ITimeValue<IValue<Integer>> valueAt = timeEvolution.getValueAt(STATE_TIME_0);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(0)));

		valueAt = timeEvolution.getValueAt(STATE_TIME_10);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(1)));

		valueAt = timeEvolution.getValueAt(STATE_TIME_20);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(0)));

		// call undo to clean up
		cmd.undo();
		
		Assert.assertEquals(ActionState.CREATED, action.getState()); 

		// and check again
		values = timeEvolution.getValues();
		Assert.assertEquals(1, values.size());

		valueAt = timeEvolution.getValueAt(STATE_TIME_0);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(0)));

	}

	/**
	 * problem specific method to create the {@link IValueChange} objects for
	 * the {@link Action} to be planned
	 * 
	 * @param action
	 *            the {@link Action} to create the {@link IValueChange} objects
	 *            for
	 */
	private void createChanges(final Action action) {

		final Parameter<Integer> parameter = action.getParameter("objective", "quantity");
		final Integer quantity = parameter.getValue();

		final IValueChange<IntegerValue> startChange = new ValueChange<>(action.getStart(), new IntegerValue(quantity));
		startChange.setStateId(STATE_INTEGER_ID);
		action.addStartChange(startChange);

		final IValueChange<IntegerValue> endChange = new ValueChange<>(action.getEnd(), new IntegerValue(-quantity));
		endChange.setStateId(STATE_INTEGER_ID);
		action.addEndChange(endChange);
	}

}
