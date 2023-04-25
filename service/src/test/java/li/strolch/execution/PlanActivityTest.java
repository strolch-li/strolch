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
package li.strolch.execution;

import static li.strolch.model.ModelGenerator.*;
import static org.junit.Assert.assertEquals;

import java.util.SortedSet;

import li.strolch.execution.command.PlanActivityCommand;
import li.strolch.model.ModelGenerator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
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
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class PlanActivityTest {

	private static final String RUNTIME_PATH = "target/" + PlanActivityTest.class.getSimpleName();
	private static final String CONFIG_SRC = "src/test/resources/executiontest";
	private static RuntimeMock runtimeMock;
	private static StrolchTransaction tx;

	@BeforeClass
	public static void beforeClass() {
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(RUNTIME_PATH, CONFIG_SRC);
		runtimeMock.startContainer();

		tx = runtimeMock.openUserTx(runtimeMock.loginTest(), false);
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	private Activity activity;
	private Resource resource1;
	private Resource resource2;
	private Action action1;
	private Action action2;

	/**
	 * initialize the resources with states and the activity with 2 actions.
	 */
	@Before
	public void init() {

		// create resource with integer state
		this.resource1 = ModelGenerator.createResource("@1", "Test With States 1", "Stated");
		IntegerTimedState timedState1 = resource1.getTimedState(STATE_INTEGER_ID);
		timedState1.getTimeEvolution().clear();
		timedState1.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);

		// create resource with integer state
		this.resource2 = ModelGenerator.createResource("@2", "Test With States 2", "Stated");
		IntegerTimedState timedState2 = resource2.getTimedState(STATE_INTEGER_ID);
		timedState2.getTimeEvolution().clear();
		timedState2.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);

		// create resource with integer state
		Resource resource3 = ModelGenerator.createResource("@3", "Test With States 3", "Stated");
		IntegerTimedState timedState3 = resource3.getTimedState(STATE_INTEGER_ID);
		timedState3.getTimeEvolution().clear();
		timedState3.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);

		// create activity element
		this.activity = new Activity("activity", "Activity", "testType", TimeOrdering.SERIES);

		// create action 1
		this.action1 = new Action("action_1", "Action 1", "Use");

		IntegerParameter iP1 = new IntegerParameter("quantity", "Occupation", 1);
		this.action1.addParameterBag(new ParameterBag("objectives", "Objectives", "Objectives"));
		this.action1.addParameter("objectives", iP1);

		createChanges(this.action1, STATE_TIME_10, STATE_TIME_20);

		this.action1.setResourceId(this.resource1.getId());
		this.action1.setResourceType(this.resource1.getType());

		this.activity.addElement(this.action1);

		// create child activity
		Activity childActivity = new Activity("childActivity", "Child Activity", "childType", TimeOrdering.SERIES);

		// create action 2
		this.action2 = new Action("action_2", "Action 2", "Use");

		IntegerParameter iP2 = new IntegerParameter("quantity", "Occupation", 1);
		this.action2.addParameterBag(new ParameterBag("objectives", "Objectives", "Objectives"));
		this.action2.addParameter("objectives", iP2);

		createChanges(this.action2, STATE_TIME_20, STATE_TIME_30);

		this.action2.setResourceId(this.resource2.getId());
		this.action2.setResourceType(this.resource2.getType());

		childActivity.addElement(this.action2);

		// create action 3
		Action action3 = new Action("action_3", "Action 3", "Use");

		IntegerParameter iP3 = new IntegerParameter("quantity", "Occupation", 1);
		action3.addParameterBag(new ParameterBag("objectives", "Objectives", "Objectives"));
		action3.addParameter("objectives", iP3);

		createChanges(action3, STATE_TIME_20, STATE_TIME_40);

		action3.setResourceId(resource3.getId());
		action3.setResourceType(resource3.getType());

		childActivity.addElement(action3);

		this.activity.addElement(childActivity);

		assertEquals(2, this.activity.getElements().size());

		tx.add(resource1);
		tx.add(resource2);
		tx.add(resource3);
		tx.add(activity);
	}

	/**
	 * The test method. Create appropriate mocks and call the command
	 */
	@Test
	public void test() {

		PlanActivityCommand planActivityCommand = new PlanActivityCommand(tx);
		planActivityCommand.setActivity(this.activity);
		planActivityCommand.doCommand();

		// check the states
		assertEquals(State.PLANNED, this.action1.getState());
		assertEquals(State.PLANNED, this.action2.getState());

		// check the resource states
		StrolchTimedState<IValue<Integer>> timedState_1 = this.resource1.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> timeEvolution_1 = timedState_1.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> values_1 = timeEvolution_1.getValues();

		assertEquals(3, values_1.size());

		ITimeValue<IValue<Integer>> valueAt_1 = timeEvolution_1.getValueAt(STATE_TIME_0);
		assertEquals(new IntegerValue(0), valueAt_1.getValue());

		valueAt_1 = timeEvolution_1.getValueAt(STATE_TIME_10);
		assertEquals(new IntegerValue(1), valueAt_1.getValue());

		valueAt_1 = timeEvolution_1.getValueAt(STATE_TIME_20);
		assertEquals(new IntegerValue(0), valueAt_1.getValue());

		// the second resource
		StrolchTimedState<IValue<Integer>> timedState_2 = this.resource2.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> timeEvolution_2 = timedState_2.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> values_2 = timeEvolution_2.getValues();

		assertEquals(3, values_2.size());

		ITimeValue<IValue<Integer>> valueAt_2 = timeEvolution_2.getValueAt(STATE_TIME_0);
		assertEquals(new IntegerValue(0), valueAt_2.getValue());

		valueAt_2 = timeEvolution_2.getValueAt(STATE_TIME_20);
		assertEquals(new IntegerValue(1), valueAt_2.getValue());

		valueAt_2 = timeEvolution_2.getValueAt(STATE_TIME_30);
		assertEquals(new IntegerValue(0), valueAt_2.getValue());
	}

	/**
	 * add changes to action start and end time with a value defined in the action objective and set the stateId of the
	 * state variable to apply the change to
	 *
	 * @param action
	 * 		the action toe create changes for
	 * @param start
	 * 		the start of the change
	 * @param end
	 * 		the end of the change
	 */
	protected static void createChanges(Action action, Long start, Long end) {

		IntegerParameter parameter = action.getParameter("objectives", "quantity");
		Integer quantity = parameter.getValue();

		IValueChange<IntegerValue> startChange = new ValueChange<>(start, new IntegerValue(quantity));
		startChange.setStateId(STATE_INTEGER_ID);
		action.addChange(startChange);

		IValueChange<IntegerValue> endChange = new ValueChange<>(end, new IntegerValue(-quantity));
		endChange.setStateId(STATE_INTEGER_ID);
		action.addChange(endChange);
	}
}
