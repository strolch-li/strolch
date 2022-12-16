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

import li.strolch.execution.command.AssignActionCommand;
import li.strolch.execution.command.PlanActionCommand;
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
public class AssignActionTest {

	private static final String RUNTIME_PATH = "target/" + AssignActionTest.class.getSimpleName();
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

	private Resource initialResource;
	private Resource targetResource;
	private Action action;

	@Before
	public void init() {

		// add initial resource with integer state variable
		this.initialResource = ModelGenerator.createResource("initial", "Test With States", "Stated");
		IntegerTimedState initialTimedState = this.initialResource.getTimedState(STATE_INTEGER_ID);
		initialTimedState.getTimeEvolution().clear();
		initialTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);

		// add target resource with integer state variable
		this.targetResource = ModelGenerator.createResource("target", "Test With States", "Stated");
		IntegerTimedState targetTimedState = this.targetResource.getTimedState(STATE_INTEGER_ID);
		targetTimedState.getTimeEvolution().clear();
		targetTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);

		Activity activity = new Activity("activity", "Test", "Test", TimeOrdering.SERIES);
		this.action = new Action("action", "Action", "Use");
		activity.addElement(this.action);

		assertEquals(State.CREATED, this.action.getState());

		IntegerParameter iP = new IntegerParameter("quantity", "Occupation", 1);
		this.action.addParameterBag(new ParameterBag("objectives", "Objectives", "Objectives"));
		this.action.addParameter("objectives", iP);

		createChanges(this.action);

		this.action.setResourceId(this.initialResource.getId());
		this.action.setResourceType(this.initialResource.getType());

		tx.add(this.initialResource);
		tx.add(this.targetResource);
		tx.add(activity);

		// finally plan the action
		PlanActionCommand planCommand = new PlanActionCommand(tx);
		planCommand.setAction(this.action);
		planCommand.doCommand();
	}

	@Test
	public void test() {

		AssignActionCommand cmd = new AssignActionCommand(tx);
		cmd.setTargetResourceId(this.targetResource.getId());
		cmd.setTargetResourceType(this.targetResource.getType());
		cmd.setAction(this.action);
		cmd.doCommand();

		// check the state
		assertEquals(State.PLANNED, this.action.getState());

		// check the resource Id
		assertEquals(this.targetResource.getId(), this.action.getResourceId());

		// check if we get the expected result
		StrolchTimedState<IValue<Integer>> initialTimedState = this.initialResource.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> initialTimeEvolution = initialTimedState.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> initialValues = initialTimeEvolution.getValues();

		assertEquals(1, initialValues.size());

		StrolchTimedState<IValue<Integer>> targetTimedState = this.targetResource.getTimedState(STATE_INTEGER_ID);
		ITimeVariable<IValue<Integer>> targetTimeEvolution = targetTimedState.getTimeEvolution();
		SortedSet<ITimeValue<IValue<Integer>>> targetValues = targetTimeEvolution.getValues();

		assertEquals(3, targetValues.size());

		ITimeValue<IValue<Integer>> valueAt = targetTimeEvolution.getValueAt(STATE_TIME_10);
		assertEquals(new IntegerValue(1), valueAt.getValue());

		valueAt = targetTimeEvolution.getValueAt(STATE_TIME_20);
		assertEquals(new IntegerValue(0), valueAt.getValue());
	}

	/**
	 * <p>
	 * add changes to action start and end time with a value defined in the action objective and set the stateId of the
	 * state variable to apply the change to
	 * </p>
	 *
	 * @param action
	 * 		the action to create the changes for
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
