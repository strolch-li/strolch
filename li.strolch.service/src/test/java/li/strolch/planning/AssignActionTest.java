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
package li.strolch.planning;

import static li.strolch.model.ModelGenerator.*;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.SortedSet;

import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.parameter.IntegerParameter;
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

	private Resource initialResource;
	private Resource targetResource;
	private Action action;
	private IntegerTimedState initialTimedState;
	private IntegerTimedState targetTimedState;
	private StrolchTransaction tx;

	@Before
	public void init() {

		// add initial resource with integer state variable
		this.initialResource = ModelGenerator.createResource("initial", "Test With States", "Stated");
		this.initialTimedState = this.initialResource.getTimedState(STATE_INTEGER_ID);
		this.initialTimedState.getTimeEvolution().clear();
		this.initialTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)),
				true);

		// add target resource with integer state variable
		this.targetResource = ModelGenerator.createResource("target", "Test With States", "Stated");
		this.targetTimedState = this.targetResource.getTimedState(STATE_INTEGER_ID);
		this.targetTimedState.getTimeEvolution().clear();
		this.targetTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)),
				true);

		this.action = new Action("action", "Action", "Use");

		Assert.assertEquals(State.CREATED, this.action.getState());

		IntegerParameter iP = new IntegerParameter("quantity", "Occupation", 1);
		this.action.addParameterBag(new ParameterBag("objective", "Objective", "Don't know"));
		this.action.addParameter("objective", iP);

		createChanges(this.action);

		this.action.setResourceId(this.initialResource.getId());
		this.action.setResourceType(this.initialResource.getType());

		this.tx = mock(StrolchTransaction.class);

		Locator locator = Locator
				.newBuilder(Tags.RESOURCE, this.initialResource.getType(), this.initialResource.getId()).build();
		when(this.tx.findElement(eq(locator))).thenReturn(this.initialResource);

		locator = Locator.newBuilder(Tags.RESOURCE, this.targetResource.getType(), this.targetResource.getId()).build();
		when(this.tx.findElement(eq(locator))).thenReturn(this.targetResource);

		// finally plan the action
		PlanActionCommand planCommand = new PlanActionCommand(null, this.tx);
		planCommand.setAction(this.action);
		planCommand.doCommand();

	}

	@Test
	public void test() {

		AssignActionCommand cmd = new AssignActionCommand(null, this.tx);
		cmd.setTargetResourceId(this.targetResource.getId());
		cmd.setTargetResourceType(this.targetResource.getType());
		cmd.setAction(this.action);
		cmd.doCommand();

		// check the state
		Assert.assertEquals(State.PLANNED, this.action.getState());

		// check the resource Id
		Assert.assertEquals(this.targetResource.getId(), this.action.getResourceId());

		// check if we get the expected result
		StrolchTimedState<IValue<Integer>> initialTimedState = this.initialResource.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> initialTimeEvolution = initialTimedState.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> initialValues = initialTimeEvolution.getValues();

		Assert.assertEquals(1, initialValues.size());

		StrolchTimedState<IValue<Integer>> targetTimedState = this.targetResource.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> targetTimeEvolution = targetTimedState.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> targetValues = targetTimeEvolution.getValues();

		Assert.assertEquals(3, targetValues.size());

		ITimeValue<IValue<Integer>> valueAt = targetTimeEvolution.getValueAt(STATE_TIME_10);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(1)));

		valueAt = targetTimeEvolution.getValueAt(STATE_TIME_20);
		Assert.assertEquals(true, valueAt.getValue().equals(new IntegerValue(0)));

		cmd.undo();

		// check the state
		Assert.assertEquals(State.PLANNED, this.action.getState());

		// check the resource Id
		Assert.assertEquals(this.initialResource.getId(), this.action.getResourceId());

		// check if we get the expected result
		targetTimedState = this.targetResource.getTimedState(STATE_INTEGER_ID);
		targetTimeEvolution = targetTimedState.getTimeEvolution();
		targetValues = targetTimeEvolution.getValues();

		Assert.assertEquals(1, targetValues.size());

		initialTimedState = this.initialResource.getTimedState(STATE_INTEGER_ID);
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
	 * add changes to action start and end time with a value defined in the action objective and set the stateId of the
	 * state variable to apply the change to
	 * </p>
	 * 
	 * @param action
	 */
	protected static void createChanges(Action action) {

		IntegerParameter parameter = action.getParameter("objective", "quantity");
		Integer quantity = parameter.getValue();

		IValueChange<IntegerValue> startChange = new ValueChange<>(STATE_TIME_10, new IntegerValue(quantity));
		startChange.setStateId(STATE_INTEGER_ID);
		action.addChange(startChange);

		IValueChange<IntegerValue> endChange = new ValueChange<>(STATE_TIME_20, new IntegerValue(-quantity));
		endChange.setStateId(STATE_INTEGER_ID);
		action.addChange(endChange);
	}
}
