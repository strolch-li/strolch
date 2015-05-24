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

import static li.strolch.model.ModelGenerator.STATE_INTEGER_ID;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_NAME;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_TIME_20;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.SortedSet;

import li.strolch.model.Locator;
import li.strolch.model.ModelGenerator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
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
import li.strolch.persistence.api.StrolchTransaction;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class AssignActionTest {

	Resource initialResource, targetResource;
	Action action;
	IntegerTimedState initialTimedState, targetTimedState;
	StrolchTransaction tx;

	@Before
	public void init() {

		// add initial resource with integer state variable
		initialResource = ModelGenerator.createResource("initial", "Test With States", "Stated");
		initialTimedState = new IntegerTimedState(STATE_INTEGER_ID, STATE_INTEGER_NAME);
		initialTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)));
		initialResource.addTimedState(initialTimedState);

		// add target resource with integer state variable
		targetResource = ModelGenerator.createResource("target", "Test With States", "Stated");
		targetTimedState = new IntegerTimedState(STATE_INTEGER_ID, STATE_INTEGER_NAME);
		targetTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)));
		targetResource.addTimedState(targetTimedState);

		action = new Action("action", "Action", "Use");

		Assert.assertEquals(State.CREATED, action.getState());

		final IntegerParameter iP = new IntegerParameter("quantity", "Occupation", 1);
		action.addParameterBag(new ParameterBag("objective", "Objective", "Don't know"));
		action.addParameter("objective", iP);

		createChanges(action);

		action.setResourceId(initialResource.getId());
		action.setResourceType(initialResource.getType());

		tx = mock(StrolchTransaction.class);

		Locator locator = Locator.newBuilder(Tags.RESOURCE, initialResource.getType(), initialResource.getId()).build();
		when(tx.findElement(eq(locator))).thenReturn(initialResource);

		locator = Locator.newBuilder(Tags.RESOURCE, targetResource.getType(), targetResource.getId()).build();
		when(tx.findElement(eq(locator))).thenReturn(targetResource);

		// finally plan the action
		final PlanActionCommand planCommand = new PlanActionCommand(null, tx);
		planCommand.setAction(action);
		planCommand.doCommand();

	}

	@Test
	public void test() {

		final AssignActionCommand cmd = new AssignActionCommand(null, tx);
		cmd.setTargetResourceId(targetResource.getId());
		cmd.setTargetResourceType(targetResource.getType());
		cmd.setAction(action);
		cmd.doCommand();

		// check the state
		Assert.assertEquals(State.PLANNED, action.getState());

		// check the resource Id
		Assert.assertEquals(targetResource.getId(), action.getResourceId());

		// check if we get the expected result
		StrolchTimedState<IValue<Integer>> initialTimedState = initialResource.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> initialTimeEvolution = initialTimedState.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> initialValues = initialTimeEvolution.getValues();

		Assert.assertEquals(1, initialValues.size());

		StrolchTimedState<IValue<Integer>> targetTimedState = targetResource.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> targetTimeEvolution = targetTimedState.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> targetValues = targetTimeEvolution.getValues();

		Assert.assertEquals(3, targetValues.size());

		ITimeValue<IValue<Integer>> valueAt = targetTimeEvolution.getValueAt(STATE_TIME_10);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(1)));

		valueAt = targetTimeEvolution.getValueAt(STATE_TIME_20);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(0)));

		cmd.undo();

		// check the state
		Assert.assertEquals(State.PLANNED, action.getState());

		// check the resource Id
		Assert.assertEquals(initialResource.getId(), action.getResourceId());

		// check if we get the expected result
		targetTimedState = targetResource.getTimedState(STATE_INTEGER_ID);
		targetTimeEvolution = targetTimedState.getTimeEvolution();
		targetValues = targetTimeEvolution.getValues();

		Assert.assertEquals(1, targetValues.size());

		initialTimedState = initialResource.getTimedState(STATE_INTEGER_ID);
		initialTimeEvolution = initialTimedState.getTimeEvolution();
		initialValues = initialTimeEvolution.getValues();

		Assert.assertEquals(3, initialValues.size());

		valueAt = initialTimeEvolution.getValueAt(STATE_TIME_10);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(1)));

		valueAt = initialTimeEvolution.getValueAt(STATE_TIME_20);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(0)));

	}

	/**
	 * <p>
	 * add changes to action start and end time with a value defined in the
	 * action objective and set the stateId of the state variable to apply the
	 * change to
	 * </p>
	 * 
	 * @param action
	 */
	protected static void createChanges(final Action action) {

		final Parameter<Integer> parameter = action.getParameter("objective", "quantity");
		final Integer quantity = parameter.getValue();

		final IValueChange<IntegerValue> startChange = new ValueChange<>(STATE_TIME_10, new IntegerValue(quantity));
		startChange.setStateId(STATE_INTEGER_ID);
		action.addChange(startChange);

		final IValueChange<IntegerValue> endChange = new ValueChange<>(STATE_TIME_20, new IntegerValue(-quantity));
		endChange.setStateId(STATE_INTEGER_ID);
		action.addChange(endChange);
	}

}
