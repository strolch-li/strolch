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

import li.strolch.execution.command.PlanActionCommand;
import li.strolch.execution.command.ShiftActionCommand;
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
public class ShiftActionTest {

	private static final String RUNTIME_PATH = "target/" + ShiftActionTest.class.getSimpleName();
	private static final String CONFIG_SRC = "src/test/resources/executiontest"; //$NON-NLS-1$
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

	private Resource resource;
	private Action action;

	@Before
	public void init() {

		// add a resource with integer state variable
		this.resource = ModelGenerator.createResource("@1", "Test With States", "Stated");
		IntegerTimedState timedState = this.resource.getTimedState(STATE_INTEGER_ID);
		timedState.getTimeEvolution().clear();
		timedState.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);

		Activity activity = new Activity("activity", "Test", "Test", TimeOrdering.SERIES);
		this.action = new Action("action", "Action", "Use");
		activity.addElement(this.action);

		assertEquals(State.CREATED, this.action.getState());

		IntegerParameter iP = new IntegerParameter("quantity", "Occupation", 1);
		this.action.addParameterBag(new ParameterBag("objectives", "Objectives", "Objectives"));
		this.action.addParameter("objectives", iP);

		createChanges(this.action);

		this.action.setResourceId(this.resource.getId());
		this.action.setResourceType(this.resource.getType());

		tx.add(this.resource);
		tx.add(activity);

		PlanActionCommand cmd = new PlanActionCommand(tx);
		cmd.setAction(this.action);
		cmd.doCommand();
	}

	@Test
	public void test() {

		ShiftActionCommand cmd = new ShiftActionCommand(tx);
		cmd.setAction(this.action);
		cmd.setShift(10L);
		cmd.doCommand();

		// check the state
		assertEquals(State.PLANNED, this.action.getState());

		// check the resource Id
		assertEquals(this.resource.getId(), this.action.getResourceId());

		// check if we get the expected result
		StrolchTimedState<IValue<Integer>> timedState = this.resource.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> timeEvolution = timedState.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> values = timeEvolution.getValues();

		assertEquals(3, values.size());

		ITimeValue<IValue<Integer>> valueAt = timeEvolution.getValueAt(STATE_TIME_0);
		assertEquals(new IntegerValue(0), valueAt.getValue());

		valueAt = timeEvolution.getValueAt(STATE_TIME_10);
		assertEquals(new IntegerValue(0), valueAt.getValue());

		valueAt = timeEvolution.getValueAt(STATE_TIME_20);
		assertEquals(new IntegerValue(1), valueAt.getValue());

		valueAt = timeEvolution.getValueAt(STATE_TIME_30);
		assertEquals(new IntegerValue(0), valueAt.getValue());
	}

	/**
	 * <p>
	 * add changes to action start and end time with a value defined in the action objective and set the stateId of the
	 * state variable to apply the change to
	 * </p>
	 *
	 * @param action
	 * 		the action to create actions for
	 */
	protected static void createChanges(Action action) {

		IntegerParameter parameter = action.getParameter("objectives", "quantity");
		Integer quantity = parameter.getValue();

		IValueChange<IntegerValue> startChange = new ValueChange<>(STATE_TIME_10, new IntegerValue(quantity));
		startChange.setStateId(STATE_INTEGER_ID);
		action.addChange(startChange);

		IValueChange<IntegerValue> endChange = new ValueChange<>(STATE_TIME_20, new IntegerValue(-quantity));
		endChange.setStateId(STATE_INTEGER_ID);
		action.addChange(endChange);
	}
}
