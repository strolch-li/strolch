package li.strolch.model.activity;

import static li.strolch.model.ModelGenerator.STATE_INTEGER_ID;
import static li.strolch.model.ModelGenerator.STATE_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_TIME_30;

import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.model.xml.ActivityToDomVisitor;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class ActionTest {

	Action action;

	/**
	 * initialize the resources with states and the activity with 2 actions.
	 */
	@Before
	public void init() {
		// create action
		this.action = new Action("action_1", "Action 1", "Use");
		this.action.setResourceId("dummyRe");
		this.action.setResourceType("dummyReType");

		IValueChange<IntegerValue> startChange = new ValueChange<>(STATE_TIME_10, new IntegerValue(1));
		startChange.setStateId(STATE_INTEGER_ID);
		this.action.addChange(startChange);

		IValueChange<IntegerValue> endChange = new ValueChange<>(STATE_TIME_30, new IntegerValue(-1));
		endChange.setStateId(STATE_INTEGER_ID);
		this.action.addChange(endChange);
	}

	@Test
	public void testGetStart() {
		Assert.assertTrue(STATE_TIME_10 == this.action.getStart());
	}

	@Test
	public void testGetEnd() {
		Assert.assertTrue(STATE_TIME_30 == this.action.getEnd());
	}

	@Test
	public void testClone() {
		Action clone = (Action) this.action.getClone();
		Assert.assertEquals(this.action.toString(), clone.toString());
		Assert.assertEquals(this.action.changes.size(), clone.changes.size());
		for (int i = 0; i < this.action.changes.size(); i++) {
			Assert.assertEquals(this.action.changes.get(i).getTime(), clone.changes.get(i).getTime());
		}
	}

	/**
	 * no test. Just to see the XML serialization in the console
	 */
	// @Test
	public void testToDOM() throws ParserConfigurationException, TransformerException {

		DocumentBuilder db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		Document document = db.newDocument();
		Element dom = new ActivityToDomVisitor().toDom(this.action);
		document.appendChild(dom);

		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		StringWriter stringWriter = new StringWriter();
		transformer.transform(new DOMSource(document), new StreamResult(stringWriter));
		String content = stringWriter.getBuffer().toString();
		System.out.println(content);
	}
}
