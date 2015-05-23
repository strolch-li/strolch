package li.strolch.model.activity;

import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class ActivityTest {

	Activity activity, childActivity;
	Action action_1, action_2, action_3;

	/**
	 * initialize the resources with states and the activity with 2 actions.
	 */
	@Before
	public void init() {

		// create activity element
		activity = new Activity("activity", "Activity", "parentType");

		// create action 1
		action_1 = new Action("action_1", "Action 1", "Use");
		action_1.setState(State.CREATED);
		action_1.setResourceType("dummyType");
		action_1.setResourceId("dummyId");

		activity.addElement(action_1);
		
		childActivity = new Activity("child_activity", "Child Activity", "childType");

		// create action 2
		action_2 = new Action("action_2", "Action 2", "Use");
		action_2.setState(State.PLANNED);
		action_2.setResourceType("dummyType");
		action_2.setResourceId("dummyId");

		childActivity.addElement(action_2);
		
		// create action 3
		action_3 = new Action("action_3", "Action 3", "Use");
		action_3.setState(State.CREATED);
		action_3.setResourceType("dummyType");
		action_3.setResourceId("dummyId");
		
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
	public void parentTests(){
		Assert.assertNull(activity.getParent());
		Assert.assertEquals(activity, activity.getRootElement()); 
		Assert.assertTrue(activity.isRootElement()); 
		
		Assert.assertEquals(activity, childActivity.getParent()); 
		Assert.assertEquals(activity, childActivity.getRootElement()); 
		Assert.assertFalse(childActivity.isRootElement());
		
		Assert.assertEquals(childActivity, action_2.getParent());
		Assert.assertEquals(activity, action_2.getRootElement()); 
		Assert.assertFalse(action_2.isRootElement()); 
	}
	
	/**
	 * no test. Just to see the XML serialization in the console
	 */
	// @Test
	public void testToDOM() throws ParserConfigurationException, TransformerException {
		
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document document = db.newDocument(); 
		Element dom = activity.toDom(document);
		document.appendChild(dom); 
		
		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		StringWriter writer = new StringWriter();
		transformer.transform(new DOMSource(document), new StreamResult(writer));
		String content = writer.getBuffer().toString();
		System.out.println(content);
		
	}
	
}
