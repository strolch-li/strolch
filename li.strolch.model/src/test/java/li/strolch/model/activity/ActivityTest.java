package li.strolch.model.activity;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ActivityTest {

	Activity activity, childActivity;
	Action action_1, action_2, action_3;

	/**
	 * initialize the resources with states and the activity with 2 actions.
	 */
	@Before
	public void init() {

		// create activity element
		activity = new Activity("activity", "Activity", "mayorType");

		// create action 1
		action_1 = new Action("action_1", "Action 1", "Use");
		action_1.setState(State.CREATED);

		activity.addElement(action_1);
		
		childActivity = new Activity("child_activity", "Child Activity", "childType");

		// create action 2
		action_2 = new Action("action_2", "Action 2", "Use");
		action_2.setState(State.PLANNED);

		childActivity.addElement(action_2);
		
		// create action 3
		action_3 = new Action("action_3", "Action 3", "Use");
		action_3.setState(State.CREATED);
		
		childActivity.addElement(action_3);
		
		activity.addElement(childActivity); 

		Assert.assertEquals(2, activity.getElements().size());
		Assert.assertEquals(2, childActivity.getElements().size());
	}

	@Test
	public void testStart() {
		Assert.assertEquals(action_1.getStart(), activity.getStart());
	}

	@Test
	public void testEnd() {
		Assert.assertEquals(action_3.getEnd(), activity.getEnd());
	}

	@Test
	public void testState() {
		Assert.assertEquals(State.CREATED, activity.getState());
	}

	@Test (expected = StrolchException.class)
	public void testIdNull() {
		activity.addElement(new Action(null, null, null));
	}
	
	@Test (expected = StrolchException.class)
	public void testIdAlreadyExists() {
		activity.addElement(new Action("action_1", "Action 1", "Use"));
	}
	
	@Test
	public void getElementTest(){
		Assert.assertNull(activity.getElement("not contained"));
		Assert.assertEquals(action_1, activity.getElement(action_1.getId()));
	}

	@Test
	public void cloneTest(){
		Activity clone = (Activity) activity.getClone();
		Assert.assertEquals(activity.toString(), clone.toString());
		Assert.assertEquals(activity.getElements().size(), clone.getElements().size());
	}
	
	@Test
	public void getParentTest(){
		Assert.assertNull(activity.getParent());
		Assert.assertNull(activity.getRootElement()); 
		Assert.assertTrue(activity.isRootElement()); 
	}
	
}
