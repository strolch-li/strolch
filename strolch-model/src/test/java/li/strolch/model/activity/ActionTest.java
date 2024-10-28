package li.strolch.model.activity;

import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.model.xml.StrolchElementToDomVisitor;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.IOException;
import java.io.StringWriter;

import static li.strolch.model.ModelGenerator.*;
import static li.strolch.utils.helper.XmlHelper.getDocumentBuilder;
import static org.junit.Assert.assertEquals;

public class ActionTest {

	private static final Logger logger = LoggerFactory.getLogger(ActionTest.class);

	private Action action;

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
		assertEquals(STATE_TIME_10, (long) this.action.getStart());
	}

	@Test
	public void testGetEnd() {
		assertEquals(STATE_TIME_30, (long) this.action.getEnd());
	}

	@Test
	public void testClone() {
		Action clone = this.action.getClone();
		assertEquals(this.action.toString(), clone.toString());
		assertEquals(this.action.changes.size(), clone.changes.size());
		for (int i = 0; i < this.action.changes.size(); i++) {
			assertEquals(this.action.changes.get(i).getTime(), clone.changes.get(i).getTime());
		}
	}

	/**
	 * no test. Just to see the XML serialization in the console
	 */
	// @Test
	public void showToDOM() throws ParserConfigurationException, TransformerException {

		Document document = getDocumentBuilder().newDocument();
		Element dom = new StrolchElementToDomVisitor().toDom(this.action);
		document.appendChild(dom);

		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		String content;
		try (StringWriter stringWriter = new StringWriter()) {
			transformer.transform(new DOMSource(document), new StreamResult(stringWriter));
			content = stringWriter.getBuffer().toString();
			logger.info(content);
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}
}
