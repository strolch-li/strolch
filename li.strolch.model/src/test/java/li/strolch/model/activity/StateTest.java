package li.strolch.model.activity;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Before;
import org.junit.Test;

import li.strolch.model.ModelGenerator;
import li.strolch.model.State;

public class StateTest {

	private Activity activity;
	private Activity subActivity;
	private Activity subSubActivity;
	private Action action;
	private Action subAction;
	private Action subSubAction1;
	private Action subSubAction2;

	@Before
	public void before() {

		this.activity = ModelGenerator.createActivity("1", "Activity 1", "ToStock", TimeOrdering.SERIES);
		assertNotNull(this.activity);

		this.action = this.activity.getElement("action_1");
		assertNotNull(this.action);

		this.subActivity = this.activity.getElement("sub_1");
		assertNotNull(this.subActivity);

		this.subAction = this.subActivity.getElement("action_1");
		assertNotNull(this.subAction);

		this.subSubActivity = this.subActivity.getElement("subSub_1");
		assertNotNull(this.subSubActivity);

		this.subSubAction1 = this.subSubActivity.getElement("action1_1");
		assertNotNull(this.subSubAction1);

		this.subSubAction2 = this.subSubActivity.getElement("action2_1");
		assertNotNull(this.subSubAction2);
	}

	@Test
	public void shouldTestCreated() {
		assertEquals(State.CREATED, this.activity.getState());

		this.action.setState(State.PLANNED);
		assertEquals(State.PLANNING, this.activity.getState());

		this.subAction.setState(State.EXECUTED);
		assertEquals(State.CREATED, this.subSubActivity.getState());
	}

	@Test
	public void shouldTestPlanning() {

		this.action.setState(State.PLANNED);
		assertEquals(State.PLANNING, this.activity.getState());

		this.action.setState(State.PLANNING);
		assertEquals(State.PLANNING, this.activity.getState());

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.PLANNING);
		this.subSubAction1.setState(State.PLANNING);
		assertEquals(State.PLANNING, this.activity.getState());

		this.subSubAction1.setState(State.CREATED);
		assertEquals(State.PLANNING, this.activity.getState());
		assertEquals(State.PLANNING, this.subActivity.getState());
		assertEquals(State.CREATED, this.subSubActivity.getState());
	}

	@Test
	public void shouldTestPlanned() {

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.PLANNING);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.PLANNING, this.activity.getState());
		assertEquals(State.PLANNING, this.subActivity.getState());
		assertEquals(State.PLANNED, this.subSubActivity.getState());

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.PLANNING);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNING);

		assertEquals(State.PLANNING, this.activity.getState());
		assertEquals(State.PLANNING, this.subActivity.getState());
		assertEquals(State.PLANNING, this.subSubActivity.getState());
	}

	@Test
	public void shouldTestExecution() {

		this.action.setState(State.EXECUTION);
		this.subAction.setState(State.PLANNING);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.PLANNING, this.subActivity.getState());
		assertEquals(State.PLANNED, this.subSubActivity.getState());

		this.action.setState(State.EXECUTION);
		this.subAction.setState(State.EXECUTION);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.PLANNED, this.subSubActivity.getState());

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.EXECUTION);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.PLANNED, this.subSubActivity.getState());

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.EXECUTED);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNING);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.PLANNING, this.subSubActivity.getState());

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.PLANNED);
		this.subSubAction1.setState(State.EXECUTION);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.PLANNED);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.EXECUTION);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.EXECUTED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.EXECUTED, this.subSubActivity.getState());

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.EXECUTED);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.EXECUTED);

		assertEquals(State.EXECUTED, this.activity.getState());
		assertEquals(State.EXECUTED, this.subActivity.getState());
		assertEquals(State.EXECUTED, this.subSubActivity.getState());

		this.action.setState(State.CREATED);
		this.subAction.setState(State.CREATED);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.EXECUTED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.EXECUTED, this.subSubActivity.getState());

		this.action.setState(State.CREATED);
		this.subAction.setState(State.CREATED);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());

		this.action.setState(State.CREATED);
		this.subAction.setState(State.CREATED);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.EXECUTED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.EXECUTED, this.subSubActivity.getState());

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.PLANNING);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());
	}

	@Test
	public void shouldTestWarning() {

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.EXECUTED);
		this.subSubAction1.setState(State.WARNING);

		assertEquals(State.WARNING, this.activity.getState());
		assertEquals(State.WARNING, this.subActivity.getState());
		assertEquals(State.WARNING, this.subSubActivity.getState());

		this.action.setState(State.WARNING);
		this.subAction.setState(State.EXECUTED);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.EXECUTED);

		assertEquals(State.WARNING, this.activity.getState());
		assertEquals(State.EXECUTED, this.subActivity.getState());
		assertEquals(State.EXECUTED, this.subSubActivity.getState());

		this.action.setState(State.EXECUTION);
		this.subAction.setState(State.WARNING);
		this.subSubAction1.setState(State.EXECUTION);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.WARNING, this.activity.getState());
		assertEquals(State.WARNING, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.WARNING);
		this.subSubAction1.setState(State.EXECUTION);
		this.subSubAction2.setState(State.EXECUTION);

		assertEquals(State.WARNING, this.activity.getState());
		assertEquals(State.WARNING, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());
	}

	@Test
	public void shouldTestError() {

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.EXECUTED);
		this.subSubAction1.setState(State.ERROR);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.ERROR, this.activity.getState());
		assertEquals(State.ERROR, this.subActivity.getState());
		assertEquals(State.ERROR, this.subSubActivity.getState());

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.ERROR);
		this.subSubAction1.setState(State.WARNING);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.ERROR, this.activity.getState());
		assertEquals(State.ERROR, this.subActivity.getState());
		assertEquals(State.WARNING, this.subSubActivity.getState());

		this.action.setState(State.ERROR);
		this.subAction.setState(State.EXECUTED);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.ERROR, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());

		this.action.setState(State.EXECUTION);
		this.subAction.setState(State.ERROR);
		this.subSubAction1.setState(State.EXECUTION);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.ERROR, this.activity.getState());
		assertEquals(State.ERROR, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.ERROR);
		this.subSubAction1.setState(State.EXECUTION);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.ERROR, this.activity.getState());
		assertEquals(State.ERROR, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());
	}

	@Test
	public void shouldTestStopped() {

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.EXECUTED);
		this.subSubAction1.setState(State.STOPPED);

		assertEquals(State.STOPPED, this.activity.getState());
		assertEquals(State.STOPPED, this.subActivity.getState());
		assertEquals(State.STOPPED, this.subSubActivity.getState());

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.STOPPED);
		this.subSubAction1.setState(State.WARNING);

		assertEquals(State.WARNING, this.activity.getState());
		assertEquals(State.WARNING, this.subActivity.getState());
		assertEquals(State.WARNING, this.subSubActivity.getState());

		this.action.setState(State.STOPPED);
		this.subAction.setState(State.EXECUTED);
		this.subSubAction1.setState(State.EXECUTED);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.STOPPED, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());

		this.action.setState(State.EXECUTION);
		this.subAction.setState(State.STOPPED);
		this.subSubAction1.setState(State.EXECUTION);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.STOPPED, this.activity.getState());
		assertEquals(State.STOPPED, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());

		this.action.setState(State.PLANNED);
		this.subAction.setState(State.STOPPED);
		this.subSubAction1.setState(State.EXECUTION);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.STOPPED, this.activity.getState());
		assertEquals(State.STOPPED, this.subActivity.getState());
		assertEquals(State.EXECUTION, this.subSubActivity.getState());

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.WARNING);
		this.subSubAction1.setState(State.ERROR);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.ERROR, this.activity.getState());
		assertEquals(State.ERROR, this.subActivity.getState());
		assertEquals(State.ERROR, this.subSubActivity.getState());

		this.action.setState(State.EXECUTED);
		this.subAction.setState(State.ERROR);
		this.subSubAction1.setState(State.WARNING);
		this.subSubAction2.setState(State.CREATED);

		assertEquals(State.ERROR, this.activity.getState());
		assertEquals(State.ERROR, this.subActivity.getState());
		assertEquals(State.WARNING, this.subSubActivity.getState());
	}

	@Test
	public void shouldTestClosed() {

		this.action.setState(State.CLOSED);
		this.subAction.setState(State.PLANNING);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.PLANNING, this.activity.getState());
		assertEquals(State.PLANNING, this.subActivity.getState());
		assertEquals(State.PLANNED, this.subSubActivity.getState());

		this.action.setState(State.CLOSED);
		this.subAction.setState(State.EXECUTION);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.PLANNED, this.subSubActivity.getState());

		this.action.setState(State.CLOSED);
		this.subAction.setState(State.EXECUTED);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.EXECUTION, this.activity.getState());
		assertEquals(State.EXECUTION, this.subActivity.getState());
		assertEquals(State.PLANNED, this.subSubActivity.getState());

		this.action.setState(State.CLOSED);
		this.subAction.setState(State.CLOSED);
		this.subSubAction1.setState(State.PLANNED);
		this.subSubAction2.setState(State.PLANNED);

		assertEquals(State.PLANNING, this.activity.getState());
		assertEquals(State.PLANNING, this.subActivity.getState());
		assertEquals(State.PLANNED, this.subSubActivity.getState());
	}
}
