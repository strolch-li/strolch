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

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.SortedSet;

import li.strolch.model.StrolchElement;
import li.strolch.model.Tags;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.impl.AString;
import li.strolch.model.timevalue.impl.StringSetValue;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StringSetTimedState extends AbstractStrolchTimedState<StringSetValue> {

	private static final long serialVersionUID = 1L;

	public static final String TYPE = "StringSetState";

	public StringSetTimedState() {
		super();
	}

	public StringSetTimedState(String id, String name) {
		super(id, name);
	}

	public StringSetTimedState(Element element) {
		super.fromDom(element);

		this.state = new TimedState<>();

		NodeList timeValueElems = element.getElementsByTagName(Tags.VALUE);
		for (int i = 0; i < timeValueElems.getLength(); i++) {
			Element timeValueElem = (Element) timeValueElems.item(i);
			Long time = Long.valueOf(timeValueElem.getAttribute(Tags.TIME));

			String valueAsString = timeValueElem.getAttribute(Tags.VALUE);
			Set<AString> value = new HashSet<>();
			String[] values = valueAsString.split(",");
			for (String s : values) {
				value.add(new AString(s.trim()));
			}

			StringSetValue integerValue = new StringSetValue(value);
			this.state.getTimeEvolution().setValueAt(time, integerValue);
		}
	}

	@Override
	public Element toDom(Document doc) {

		Element stateElement = doc.createElement(Tags.TIMED_STATE);
		super.fillElement(stateElement);
		SortedSet<ITimeValue<StringSetValue>> values = this.state.getTimeEvolution().getValues();
		for (ITimeValue<StringSetValue> timeValue : values) {
			Long time = timeValue.getTime();
			StringSetValue stringSetValue = timeValue.getValue();

			Set<AString> value = stringSetValue.getValue();
			StringBuilder sb = new StringBuilder();
			Iterator<AString> iter = value.iterator();
			while (iter.hasNext()) {
				sb.append(iter.next().getString());
				if (iter.hasNext()) {
					sb.append(", ");
				}
			}
			String valueAsString = sb.toString();

			Element valueElem = doc.createElement(Tags.VALUE);
			valueElem.setAttribute(Tags.TIME, time.toString());
			valueElem.setAttribute(Tags.VALUE, valueAsString);
			stateElement.appendChild(valueElem);
		}

		return stateElement;
	}

	@Override
	public String getType() {
		return TYPE;
	}

	@Override
	public StrolchElement getClone() {
		StringSetTimedState clone = new StringSetTimedState();
		fillClone(clone);
		return clone;
	}
}
