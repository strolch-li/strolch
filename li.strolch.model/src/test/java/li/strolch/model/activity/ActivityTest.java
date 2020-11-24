/*
 * Copyright 2016 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model.activity;

import static org.junit.Assert.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import li.strolch.exception.StrolchException;
import li.strolch.model.ModelGenerator;
import li.strolch.model.State;
import li.strolch.model.xml.StrolchElementToDomVisitor;
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
		this.activity = new Activity("activity", "Activity", "parentType", TimeOrdering.SERIES);

		// create action 1
		this.action_1 = new Action("action_1", "Action 1", "Use");
		this.action_1.setState(State.CREATED);
		this.action_1.setResourceType("dummyType");
		this.action_1.setResourceId("dummyId");

		this.activity.addElement(this.action_1);

		this.childActivity = new Activity("child_activity", "Child Activity", "childType", TimeOrdering.SERIES);

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

		assertEquals(2, this.activity.getElements().size());
		assertEquals(2, this.childActivity.getElements().size());
	}

	@Test
	public void testStart() {
		assertEquals(this.action_1.getStart(), this.activity.getStart());
	}

	@Test
	public void testEnd() {
		assertEquals(this.action_3.getEnd(), this.activity.getEnd());
	}

	@Test
	public void testState() {
		assertEquals(State.PLANNING, this.activity.getState());
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
	public void testPreviousNext() {

		Activity a = ModelGenerator.createActivity("bla", "Bla", "Bla", TimeOrdering.SERIES);

		IActivityElement element = a.getElement("action_bla");
		assertNotNull(element);
		Optional<IActivityElement> previousElement = a.getPreviousElement(element);
		Optional<IActivityElement> nextElement = a.getNextElement(element);
		assertFalse(previousElement.isPresent());
		assertEquals("sub_bla", nextElement.get().getId());

		element = a.getElement("sub_bla");
		assertNotNull(element);
		previousElement = a.getPreviousElement(element);
		nextElement = a.getNextElement(element);
		assertEquals("action_bla", previousElement.get().getId());
		assertFalse(nextElement.isPresent());
	}

	@Test
	public void testPreviousNext1() {

		Activity a = new Activity("bla", "Bla", "Bla", TimeOrdering.SERIES);

		Action a0 = new Action("a0", "A0", "Wait");
		a.addElement(a0);
		Action a1 = new Action("a1", "A1", "Consume");
		a.addElement(a1);
		Action a2 = new Action("a2", "A2", "Consume");
		a.addElement(a2);
		Action a3 = new Action("a3", "A3", "Produce");
		a.addElement(a3);
		Action a4 = new Action("a4", "A4", "Produce");
		a.addElement(a4);
		Action a5 = new Action("a5", "A5", "Use");
		a.addElement(a5);

		Optional<IActivityElement> previousElement = a.getPreviousElement(a0);
		assertFalse(previousElement.isPresent());
		Optional<IActivityElement> nextElement = a.getNextElement(a0);
		assertEquals("a1", nextElement.get().getId());

		previousElement = a.getPreviousElement(a1);
		assertEquals("a0", previousElement.get().getId());
		nextElement = a.getNextElement(a1);
		assertEquals("a2", nextElement.get().getId());

		previousElement = a.getPreviousElement(a2);
		nextElement = a.getNextElement(a2);
		assertEquals("a1", previousElement.get().getId());
		assertEquals("a3", nextElement.get().getId());

		previousElement = a.getPreviousElement(a3);
		nextElement = a.getNextElement(a3);
		assertEquals("a2", previousElement.get().getId());
		assertEquals("a4", nextElement.get().getId());

		previousElement = a.getPreviousElement(a4);
		nextElement = a.getNextElement(a4);
		assertEquals("a3", previousElement.get().getId());
		assertEquals("a5", nextElement.get().getId());

		previousElement = a.getPreviousElement(a5);
		nextElement = a.getNextElement(a5);
		assertEquals("a4", previousElement.get().getId());
		assertFalse(nextElement.isPresent());
	}

	@Test
	public void testPreviousNextByType() {

		Activity a = new Activity("bla", "Bla", "Bla", TimeOrdering.SERIES);

		Action a0 = new Action("a0", "A0", "Wait");
		a.addElement(a0);
		Action a1 = new Action("a1", "A1", "Consume");
		a.addElement(a1);
		Action a2 = new Action("a2", "A2", "Consume");
		a.addElement(a2);
		Action a3 = new Action("a3", "A3", "Produce");
		a.addElement(a3);
		Action a4 = new Action("a4", "A4", "Produce");
		a.addElement(a4);
		Action a5 = new Action("a5", "A5", "Use");
		a.addElement(a5);

		// a0
		Optional<IActivityElement> nextElement = a.getNextElementByType(a0, "Consume");
		assertEquals("a1", nextElement.get().getId());
		nextElement = a.getNextElementByType(a0, "Produce");
		assertEquals("a3", nextElement.get().getId());

		// a1
		Optional<IActivityElement> previousElement = a.getPreviousElementByType(a1, "Consume");
		assertFalse(previousElement.isPresent());
		nextElement = a.getNextElementByType(a1, "Consume");
		assertEquals("a2", nextElement.get().getId());
		nextElement = a.getNextElementByType(a1, "Produce");
		assertEquals("a3", nextElement.get().getId());

		// a2
		previousElement = a.getPreviousElementByType(a2, "Wait");
		assertEquals("a0", previousElement.get().getId());
		previousElement = a.getPreviousElementByType(a2, "Consume");
		assertEquals("a1", previousElement.get().getId());
		nextElement = a.getNextElementByType(a2, "Consume");
		assertFalse(nextElement.isPresent());
		nextElement = a.getNextElementByType(a2, "Produce");
		assertEquals("a3", nextElement.get().getId());
		nextElement = a.getNextElementByType(a2, "Use");
		assertEquals("a5", nextElement.get().getId());

		// a3
		previousElement = a.getPreviousElementByType(a3, "Consume");
		assertEquals("a2", previousElement.get().getId());
		nextElement = a.getNextElementByType(a3, "Produce");
		assertEquals("a4", nextElement.get().getId());

		// a4
		previousElement = a.getPreviousElementByType(a4, "Produce");
		assertEquals("a3", previousElement.get().getId());
		nextElement = a.getNextElementByType(a4, "Produce");
		assertFalse(nextElement.isPresent());
		nextElement = a.getNextElementByType(a4, "Use");
		assertEquals("a5", nextElement.get().getId());

		// a5
		previousElement = a.getPreviousElementByType(a5, "Produce");
		assertEquals("a4", previousElement.get().getId());
		previousElement = a.getPreviousElementByType(a5, "Consume");
		assertEquals("a2", previousElement.get().getId());
		nextElement = a.getNextElementByType(a5, "Produce");
		assertFalse(nextElement.isPresent());
		nextElement = a.getNextElementByType(a5, "Use");
		assertFalse(nextElement.isPresent());
	}

	@Test
	public void testAddAfter() {
		Activity a = new Activity("bla", "Bla", "Bla", TimeOrdering.SERIES);

		Action a0 = new Action("a0", "A0", "Wait");
		a.addElement(a0);
		Action a1 = new Action("a1", "A1", "Consume");
		a.addElement(a1);
		Action a2 = new Action("a2", "A2", "Consume");
		a.addElement(a2);

		Action a3 = new Action("a3", "A3", "Produce");
		a.addElementAfter(a0, a3);

		Iterator<Map.Entry<String, IActivityElement>> iter = a.elementIterator();
		assertEquals(a0, iter.next().getValue());
		assertEquals(a3, iter.next().getValue());
		assertEquals(a1, iter.next().getValue());
		assertEquals(a2, iter.next().getValue());
	}

	@Test
	public void testAddBefore() {
		Activity a = new Activity("bla", "Bla", "Bla", TimeOrdering.SERIES);

		Action a0 = new Action("a0", "A0", "Wait");
		a.addElement(a0);
		Action a1 = new Action("a1", "A1", "Consume");
		a.addElement(a1);
		Action a2 = new Action("a2", "A2", "Consume");
		a.addElement(a2);

		Action a3 = new Action("a3", "A3", "Produce");
		a.addElementBefore(a1, a3);

		Iterator<Map.Entry<String, IActivityElement>> iter = a.elementIterator();
		assertEquals(a0, iter.next().getValue());
		assertEquals(a3, iter.next().getValue());
		assertEquals(a1, iter.next().getValue());
		assertEquals(a2, iter.next().getValue());
	}

	@Test
	public void testActionsWithState() {

		Activity a = ModelGenerator.createActivity("bla", "Bla", "Bla", TimeOrdering.SERIES);

		List<Action> actionsWithState = a.getActionsWithState(State.CREATED);
		assertEquals(4, actionsWithState.size());

		Action action = a.getElement("action_bla");
		action.setState(State.EXECUTED);
		actionsWithState = a.getActionsWithState(State.CREATED);
		assertEquals(3, actionsWithState.size());
		actionsWithState = a.getActionsWithState(State.EXECUTED);
		assertEquals(1, actionsWithState.size());
		assertEquals("action_bla", actionsWithState.get(0).getId());
	}

	@Test
	public void getElementTest() {
		assertEquals(this.action_1, this.activity.getElement(this.action_1.getId()));
	}

	@Test
	public void cloneTest() {
		Activity clone = this.activity.getClone();
		assertEquals(this.activity.toString(), clone.toString());
		assertEquals(this.activity.getElements().size(), clone.getElements().size());
	}

	@Test
	public void parentTests() {
		Assert.assertNull(this.activity.getParent());
		assertEquals(this.activity, this.activity.getRootElement());
		Assert.assertTrue(this.activity.isRootElement());

		assertEquals(this.activity, this.childActivity.getParent());
		assertEquals(this.activity, this.childActivity.getRootElement());
		Assert.assertFalse(this.childActivity.isRootElement());

		assertEquals(this.childActivity, this.action_2.getParent());
		assertEquals(this.activity, this.action_2.getRootElement());
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
		Element dom = new StrolchElementToDomVisitor().toDom(this.activity);
		document.appendChild(dom);

		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		StringWriter writer = new StringWriter();
		transformer.transform(new DOMSource(document), new StreamResult(writer));
		String content = writer.getBuffer().toString();
		System.out.println(content);
	}
}
