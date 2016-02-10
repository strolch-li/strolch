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

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import li.strolch.exception.StrolchException;
import li.strolch.model.State;
import li.strolch.model.xml.ActivityToDomVisitor;

public class ActivityTest {

	Activity activity, childActivity;
	Action action_1, action_2, action_3;

	/**
	 * initialize the resources with states and the activity with 2 actions.
	 */
	@Before
	public void init() {

		// create activity element
		this.activity = new Activity("activity", "Activity", "parentType");

		// create action 1
		this.action_1 = new Action("action_1", "Action 1", "Use");
		this.action_1.setState(State.CREATED);
		this.action_1.setResourceType("dummyType");
		this.action_1.setResourceId("dummyId");

		this.activity.addElement(this.action_1);

		this.childActivity = new Activity("child_activity", "Child Activity", "childType");

		// create action 2
		this.action_2 = new Action("action_2", "Action 2", "Use");
		this.action_2.setState(State.PLANNED);
		this.action_2.setResourceType("dummyType");
		this.action_2.setResourceId("dummyId");

		this.childActivity.addElement(this.action_2);

		// create action 3
		this.action_3 = new Action("action_3", "Action 3", "Use");
		this.action_3.setState(State.CREATED);
		this.action_3.setResourceType("dummyType");
		this.action_3.setResourceId("dummyId");

		this.childActivity.addElement(this.action_3);

		this.activity.addElement(this.childActivity);

		Assert.assertEquals(2, this.activity.getElements().size());
		Assert.assertEquals(2, this.childActivity.getElements().size());
	}

	@Test
	public void testStart() {
		Assert.assertEquals(this.action_1.getStart(), this.activity.getStart());
	}

	@Test
	public void testEnd() {
		Assert.assertEquals(this.action_3.getEnd(), this.activity.getEnd());
	}

	@Test
	public void testState() {
		Assert.assertEquals(State.CREATED, this.activity.getState());
	}

	@Test(expected = StrolchException.class)
	public void testIdNull() {
		this.activity.addElement(new Action(null, null, null));
	}

	@Test(expected = StrolchException.class)
	public void testIdAlreadyExists() {
		this.activity.addElement(new Action("action_1", "Action 1", "Use"));
	}

	@Test
	public void getElementTest() {
		Assert.assertNull(this.activity.getElement("not contained"));
		Assert.assertEquals(this.action_1, this.activity.getElement(this.action_1.getId()));
	}

	@Test
	public void cloneTest() {
		Activity clone = this.activity.getClone();
		Assert.assertEquals(this.activity.toString(), clone.toString());
		Assert.assertEquals(this.activity.getElements().size(), clone.getElements().size());
	}

	@Test
	public void parentTests() {
		Assert.assertNull(this.activity.getParent());
		Assert.assertEquals(this.activity, this.activity.getRootElement());
		Assert.assertTrue(this.activity.isRootElement());

		Assert.assertEquals(this.activity, this.childActivity.getParent());
		Assert.assertEquals(this.activity, this.childActivity.getRootElement());
		Assert.assertFalse(this.childActivity.isRootElement());

		Assert.assertEquals(this.childActivity, this.action_2.getParent());
		Assert.assertEquals(this.activity, this.action_2.getRootElement());
		Assert.assertFalse(this.action_2.isRootElement());
	}

	/**
	 * no test. Just to see the XML serialization in the console
	 */
	// @Test
	public void testToDOM() throws ParserConfigurationException, TransformerException {

		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = dbf.newDocumentBuilder();
		Document document = db.newDocument();
		Element dom = new ActivityToDomVisitor().toDom(this.activity);
		document.appendChild(dom);

		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		StringWriter writer = new StringWriter();
		transformer.transform(new DOMSource(document), new StreamResult(writer));
		String content = writer.getBuffer().toString();
		System.out.println(content);
	}
}
