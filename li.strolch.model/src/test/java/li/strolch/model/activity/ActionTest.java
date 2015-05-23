package li.strolch.model.activity;

import static li.strolch.model.ModelGenerator.STATE_INTEGER_ID;
import static li.strolch.model.ModelGenerator.STATE_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_TIME_30;

import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.ValueChange;

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
		action = new Action("action_1", "Action 1", "Use");
		
		IValueChange<IntegerValue> startChange = new ValueChange<>(STATE_TIME_10, new IntegerValue(1));
		startChange.setStateId(STATE_INTEGER_ID);
		action.addChange(startChange);

		IValueChange<IntegerValue> endChange = new ValueChange<>(STATE_TIME_30, new IntegerValue(-1));
		endChange.setStateId(STATE_INTEGER_ID);
		action.addChange(endChange);
	}
	
	@Test
	public void testGetStart() {
		Assert.assertTrue(STATE_TIME_10 == action.getStart());
	}
	
	@Test
	public void testGetEnd() {
		Assert.assertTrue(STATE_TIME_30 == action.getEnd());
	}
	

	@Test
	public void testClone() {
		Action clone = (Action) action.getClone();
		Assert.assertEquals(action.toString(), clone.toString());
		Assert.assertEquals(action.changes.size(), clone.changes.size());
		for (int i = 0; i < action.changes.size(); i++) {
			Assert.assertEquals(action.changes.get(i).getTime(), clone.changes.get(i).getTime());
		}
	}
	
	@Test
	public void testToDOM() throws ParserConfigurationException, TransformerException {
		
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document document = db.newDocument(); 
		Element dom = action.toDom(document);
		document.appendChild(dom); 
		
		TransformerFactory tf = TransformerFactory.newInstance();
		Transformer transformer = tf.newTransformer();
		transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
		StringWriter writer = new StringWriter();
		transformer.transform(new DOMSource(document), new StreamResult(writer));
		String content = writer.getBuffer().toString();
		System.out.println(content);
		
	}
	
}
