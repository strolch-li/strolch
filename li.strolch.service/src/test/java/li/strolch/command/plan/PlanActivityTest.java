package li.strolch.command.plan;

import static li.strolch.model.ModelGenerator.STATE_INTEGER_ID;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_NAME;
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

import li.strolch.model.Locator;
import li.strolch.model.ModelGenerator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
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
 * 
 * @author Martin Smock <martin.smock@bluewin.ch>
 * 
 */
public class PlanActivityTest {

	Activity activity, childActivity;
	Resource resource_1, resource_2, resource_3;
	IntegerTimedState timedState_1, timedState_2, timedState_3;
	Action action_1, action_2, action_3;

	/**
	 * initialize the resources with states and the activity with 2 actions.
	 */
	@Before
	public void init() {

		// create resource with integer state
		resource_1 = ModelGenerator.createResource("@1", "Test With States 1", "Stated");
		timedState_1 = new IntegerTimedState(STATE_INTEGER_ID, STATE_INTEGER_NAME);
		timedState_1.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)));
		resource_1.addTimedState(timedState_1);

		// create resource with integer state
		resource_2 = ModelGenerator.createResource("@2", "Test With States 2", "Stated");
		timedState_2 = new IntegerTimedState(STATE_INTEGER_ID, STATE_INTEGER_NAME);
		timedState_2.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)));
		resource_2.addTimedState(timedState_2);
		
		// create resource with integer state
		resource_3 = ModelGenerator.createResource("@3", "Test With States 3", "Stated");
		timedState_3 = new IntegerTimedState(STATE_INTEGER_ID, STATE_INTEGER_NAME);
		timedState_3.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)));
		resource_3.addTimedState(timedState_3);

		// create activity element
		activity = new Activity("activity", "Activity", "testType");

		// create action 1
		action_1 = new Action("action_1", "Action 1", "Use");
		action_1.setStart(STATE_TIME_10);
		action_1.setEnd(STATE_TIME_20);

		IntegerParameter iP1 = new IntegerParameter("quantity", "Occupation", 1);
		action_1.addParameterBag(new ParameterBag("objective", "Objective", "Don't know"));
		action_1.addParameter("objective", iP1);

		createChanges(action_1);

		action_1.setResourceId(resource_1.getId());
		action_1.setResourceType(resource_1.getType());

		activity.addElement(action_1);
		
		// create child activity 
		childActivity = new Activity("childActivity", "Child Activity", "childType");

		// create action 2
		action_2 = new Action("action_2", "Action 2", "Use");
		action_2.setStart(STATE_TIME_20);
		action_2.setEnd(STATE_TIME_30);

		IntegerParameter iP2 = new IntegerParameter("quantity", "Occupation", 1);
		action_2.addParameterBag(new ParameterBag("objective", "Objective", "Don't know"));
		action_2.addParameter("objective", iP2);

		createChanges(action_2);

		action_2.setResourceId(resource_2.getId());
		action_2.setResourceType(resource_2.getType());

		childActivity.addElement(action_2);
		
		// create action 3
		action_3 = new Action("action_3", "Action 3", "Use");
		action_3.setStart(STATE_TIME_20);
		action_3.setEnd(STATE_TIME_40);

		IntegerParameter iP3 = new IntegerParameter("quantity", "Occupation", 1);
		action_3.addParameterBag(new ParameterBag("objective", "Objective", "Don't know"));
		action_3.addParameter("objective", iP3);

		createChanges(action_3);

		action_3.setResourceId(resource_3.getId());
		action_3.setResourceType(resource_3.getType());

		childActivity.addElement(action_3);
		
		activity.addElement(childActivity); 

		Assert.assertEquals(2, activity.getElements().size());

	}

	/**
	 * The test method. Create appropriate mocks and call the command
	 */
	@Test
	public void test() {

		StrolchTransaction tx = mock(StrolchTransaction.class);

		Locator locator1 = Locator.newBuilder(Tags.RESOURCE, "Stated", "@1").build();
		when(tx.findElement(eq(locator1))).thenReturn(resource_1);

		Locator locator2 = Locator.newBuilder(Tags.RESOURCE, "Stated", "@2").build();
		when(tx.findElement(eq(locator2))).thenReturn(resource_2);

		Locator locator3 = Locator.newBuilder(Tags.RESOURCE, "Stated", "@3").build();
		when(tx.findElement(eq(locator3))).thenReturn(resource_3);
		
		PlanActivityCommand planActivityCommand = new PlanActivityCommand(null, tx);
		planActivityCommand.setActivity(activity);
		planActivityCommand.doCommand();

		// check the states
		Assert.assertEquals(State.PLANNED, action_1.getState());
		Assert.assertEquals(State.PLANNED, action_2.getState());

		// check the resource states
		StrolchTimedState<IValue<Integer>> timedState_1 = resource_1.getTimedState(STATE_INTEGER_ID);
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
		StrolchTimedState<IValue<Integer>> timedState_2 = resource_2.getTimedState(STATE_INTEGER_ID);
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
		Assert.assertEquals(State.CREATED, action_1.getState());
		Assert.assertEquals(State.CREATED, action_2.getState());

		// check the resource states
		values_1 = timeEvolution_1.getValues();
		Assert.assertEquals(1, values_1.size());

		values_2 = timeEvolution_2.getValues();
		Assert.assertEquals(1, values_2.size());

	}

	/**
	 * add changes to action start and end time with a value defined in the
	 * action objective and set the stateId of the state variable to apply the
	 * change to
	 */
	protected static void createChanges(final Action action) {

		Parameter<Integer> parameter = action.getParameter("objective", "quantity");
		Integer quantity = parameter.getValue();

		IValueChange<IntegerValue> startChange = new ValueChange<>(action.getStart(), new IntegerValue(quantity));
		startChange.setStateId(STATE_INTEGER_ID);
		action.addStartChange(startChange);

		IValueChange<IntegerValue> endChange = new ValueChange<>(action.getEnd(), new IntegerValue(-quantity));
		endChange.setStateId(STATE_INTEGER_ID);
		action.addEndChange(endChange);

	}

}
