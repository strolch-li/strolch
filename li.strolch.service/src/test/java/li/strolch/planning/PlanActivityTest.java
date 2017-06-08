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

import static li.strolch.model.ModelGenerator.STATE_INTEGER_ID;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_TIME_20;
import static li.strolch.model.ModelGenerator.STATE_TIME_30;
import static li.strolch.model.ModelGenerator.STATE_TIME_40;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.SortedSet;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import li.strolch.model.Locator;
import li.strolch.model.ModelGenerator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
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
import li.strolch.planning.PlanActivityCommand;

/**
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class PlanActivityTest {

	private Activity activity;
	private Activity childActivity;
	private Resource resource1;
	private Resource resource2;
	private Resource resource3;
	private IntegerTimedState timedState1;
	private IntegerTimedState timedState2;
	private IntegerTimedState timedState3;
	private Action action1;
	private Action action2;
	private Action action3;
	private StrolchTransaction tx;

	/**
	 * initialize the resources with states and the activity with 2 actions.
	 */
	@Before
	public void init() {

		// create resource with integer state
		this.resource1 = ModelGenerator.createResource("@1", "Test With States 1", "Stated");
		this.timedState1 = resource1.getTimedState(STATE_INTEGER_ID);
		this.timedState1.getTimeEvolution().clear();
		this.timedState1.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);

		// create resource with integer state
		this.resource2 = ModelGenerator.createResource("@2", "Test With States 2", "Stated");
		this.timedState2 = resource2.getTimedState(STATE_INTEGER_ID);
		this.timedState2.getTimeEvolution().clear();
		this.timedState2.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);

		// create resource with integer state
		this.resource3 = ModelGenerator.createResource("@3", "Test With States 3", "Stated");
		this.timedState3 = resource3.getTimedState(STATE_INTEGER_ID);
		this.timedState3.getTimeEvolution().clear();
		this.timedState3.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);

		// create activity element
		this.activity = new Activity("activity", "Activity", "testType", TimeOrdering.SERIES);

		// create action 1
		this.action1 = new Action("action_1", "Action 1", "Use");

		IntegerParameter iP1 = new IntegerParameter("quantity", "Occupation", 1);
		this.action1.addParameterBag(new ParameterBag("objective", "Objective", "Don't know"));
		this.action1.addParameter("objective", iP1);

		createChanges(this.action1, STATE_TIME_10, STATE_TIME_20);

		this.action1.setResourceId(this.resource1.getId());
		this.action1.setResourceType(this.resource1.getType());

		this.activity.addElement(this.action1);

		// create child activity
		this.childActivity = new Activity("childActivity", "Child Activity", "childType", TimeOrdering.SERIES);

		// create action 2
		this.action2 = new Action("action_2", "Action 2", "Use");

		IntegerParameter iP2 = new IntegerParameter("quantity", "Occupation", 1);
		this.action2.addParameterBag(new ParameterBag("objective", "Objective", "Don't know"));
		this.action2.addParameter("objective", iP2);

		createChanges(this.action2, STATE_TIME_20, STATE_TIME_30);

		this.action2.setResourceId(this.resource2.getId());
		this.action2.setResourceType(this.resource2.getType());

		this.childActivity.addElement(this.action2);

		// create action 3
		this.action3 = new Action("action_3", "Action 3", "Use");

		IntegerParameter iP3 = new IntegerParameter("quantity", "Occupation", 1);
		this.action3.addParameterBag(new ParameterBag("objective", "Objective", "Don't know"));
		this.action3.addParameter("objective", iP3);

		createChanges(this.action3, STATE_TIME_20, STATE_TIME_40);

		this.action3.setResourceId(this.resource3.getId());
		this.action3.setResourceType(this.resource3.getType());

		this.childActivity.addElement(this.action3);

		this.activity.addElement(this.childActivity);

		Assert.assertEquals(2, this.activity.getElements().size());

		this.tx = mock(StrolchTransaction.class);

		Locator locator1 = Locator.newBuilder(Tags.RESOURCE, "Stated", "@1").build();
		when(this.tx.findElement(eq(locator1))).thenReturn(this.resource1);

		Locator locator2 = Locator.newBuilder(Tags.RESOURCE, "Stated", "@2").build();
		when(this.tx.findElement(eq(locator2))).thenReturn(this.resource2);

		Locator locator3 = Locator.newBuilder(Tags.RESOURCE, "Stated", "@3").build();
		when(this.tx.findElement(eq(locator3))).thenReturn(this.resource3);

	}

	/**
	 * The test method. Create appropriate mocks and call the command
	 */
	@Test
	public void test() {

		PlanActivityCommand planActivityCommand = new PlanActivityCommand(null, this.tx);
		planActivityCommand.setActivity(this.activity);
		planActivityCommand.doCommand();

		// check the states
		Assert.assertEquals(State.PLANNED, this.action1.getState());
		Assert.assertEquals(State.PLANNED, this.action2.getState());

		// check the resource states
		StrolchTimedState<IValue<Integer>> timedState_1 = this.resource1.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> timeEvolution_1 = timedState_1.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> values_1 = timeEvolution_1.getValues();

		Assert.assertEquals(3, values_1.size());

		ITimeValue<IValue<Integer>> valueAt_1 = timeEvolution_1.getValueAt(STATE_TIME_0);
		Assert.assertEquals(true, valueAt_1.getValue().equals(new IntegerValue(0)));

		valueAt_1 = timeEvolution_1.getValueAt(STATE_TIME_10);
		Assert.assertEquals(true, valueAt_1.getValue().equals(new IntegerValue(1)));

		valueAt_1 = timeEvolution_1.getValueAt(STATE_TIME_20);
		Assert.assertEquals(true, valueAt_1.getValue().equals(new IntegerValue(0)));

		// the second resource
		StrolchTimedState<IValue<Integer>> timedState_2 = this.resource2.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> timeEvolution_2 = timedState_2.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> values_2 = timeEvolution_2.getValues();

		Assert.assertEquals(3, values_2.size());

		ITimeValue<IValue<Integer>> valueAt_2 = timeEvolution_2.getValueAt(STATE_TIME_0);
		Assert.assertEquals(true, valueAt_2.getValue().equals(new IntegerValue(0)));

		valueAt_2 = timeEvolution_2.getValueAt(STATE_TIME_20);
		Assert.assertEquals(true, valueAt_2.getValue().equals(new IntegerValue(1)));

		valueAt_2 = timeEvolution_2.getValueAt(STATE_TIME_30);
		Assert.assertEquals(true, valueAt_2.getValue().equals(new IntegerValue(0)));

		// test undo function
		planActivityCommand.undo();

		// check the states
		Assert.assertEquals(State.CREATED, this.action1.getState());
		Assert.assertEquals(State.CREATED, this.action2.getState());

		// check the resource states
		values_1 = timeEvolution_1.getValues();
		Assert.assertEquals(1, values_1.size());

		values_2 = timeEvolution_2.getValues();
		Assert.assertEquals(1, values_2.size());

	}

	/**
	 * add changes to action start and end time with a value defined in the action objective and set the stateId of the
	 * state variable to apply the change to
	 */
	protected static void createChanges(Action action, Long start, Long end) {

		Parameter<Integer> parameter = action.getParameter("objective", "quantity");
		Integer quantity = parameter.getValue();

		IValueChange<IntegerValue> startChange = new ValueChange<>(start, new IntegerValue(quantity));
		startChange.setStateId(STATE_INTEGER_ID);
		action.addChange(startChange);

		IValueChange<IntegerValue> endChange = new ValueChange<>(end, new IntegerValue(-quantity));
		endChange.setStateId(STATE_INTEGER_ID);
		action.addChange(endChange);
	}
}
