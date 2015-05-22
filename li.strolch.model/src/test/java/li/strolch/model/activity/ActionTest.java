package li.strolch.model.activity;

import static li.strolch.model.ModelGenerator.STATE_INTEGER_ID;
import static li.strolch.model.ModelGenerator.STATE_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_TIME_30;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.ValueChange;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ActionTest {

	Action action;

	/**
	 * initialize the resources with states and the activity with 2 actions.
	 */
	@Before
	public void init() {
		// create action
		action = new Action("action_1", "Action 1", "Use");
		action.setStart(STATE_TIME_10);
		action.setEnd(STATE_TIME_30);
		
		IValueChange<IntegerValue> startChange = new ValueChange<>(action.getStart(), new IntegerValue(1));
		startChange.setStateId(STATE_INTEGER_ID);
		action.addStartChange(startChange);

		IValueChange<IntegerValue> endChange = new ValueChange<>(action.getEnd(), new IntegerValue(-1));
		endChange.setStateId(STATE_INTEGER_ID);
		action.addEndChange(endChange);
	}

	@Test
	public void testClone() {
		Action clone = (Action) action.getClone();
		Assert.assertEquals(action.toString(), clone.toString());
		Assert.assertEquals(action.startChanges.size(), clone.startChanges.size());
		Assert.assertEquals(action.endChanges.size(), clone.endChanges.size());
		for (int i = 0; i < action.startChanges.size(); i++) {
			Assert.assertEquals(action.startChanges.get(i).getTime(), clone.startChanges.get(i).getTime());
			Assert.assertEquals(action.startChanges.get(i).getValue(), clone.startChanges.get(i).getValue());
		}
		for (int i = 0; i < action.endChanges.size(); i++) {
			Assert.assertEquals(action.endChanges.get(i).getTime(), clone.endChanges.get(i).getTime());
			Assert.assertEquals(action.endChanges.get(i).getValue(), clone.endChanges.get(i).getValue());
		}
	}
}
