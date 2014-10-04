/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model.timedstate;

import java.util.SortedSet;

import li.strolch.model.Tags;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.impl.FloatValue;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FloatTimedState extends AbstractStrolchTimedState<FloatValue> {

	private static final long serialVersionUID = 1L;

	public static final String TYPE = "FloatState";

	public FloatTimedState() {
		super();
	}

	public FloatTimedState(String id, String name) {
		super(id, name);
	}

	public FloatTimedState(Element element) {
		super.fromDom(element);

		this.state = new TimedState<>();

		NodeList timeValueElems = element.getElementsByTagName(Tags.VALUE);
		for (int i = 0; i < timeValueElems.getLength(); i++) {
			Element timeValueElem = (Element) timeValueElems.item(i);
			Long time = Long.valueOf(timeValueElem.getAttribute(Tags.TIME));
			Double value = Double.valueOf(timeValueElem.getAttribute(Tags.VALUE));
			FloatValue floatValue = new FloatValue(value);
			this.state.getTimeEvolution().setValueAt(time, floatValue);
		}
	}

	@Override
	public Element toDom(Document doc) {

		Element stateElement = doc.createElement(Tags.TIMED_STATE);
		super.fillElement(stateElement);
		SortedSet<ITimeValue<FloatValue>> values = this.state.getTimeEvolution().getValues();
		for (ITimeValue<FloatValue> timeValue : values) {
			Long time = timeValue.getTime();
			FloatValue value = timeValue.getValue();
			Element valueElem = doc.createElement(Tags.VALUE);
			valueElem.setAttribute(Tags.TIME, time.toString());
			valueElem.setAttribute(Tags.VALUE, value.getValue().toString());
			stateElement.appendChild(valueElem);
		}

		return stateElement;
	}

	@Override
	public String getType() {
		return TYPE;
	}

	@Override
	public FloatTimedState getClone() {
		FloatTimedState clone = new FloatTimedState();
		fillClone(clone);
		return clone;
	}
}
