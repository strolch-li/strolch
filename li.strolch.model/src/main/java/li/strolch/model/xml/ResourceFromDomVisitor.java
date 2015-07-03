/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model.xml;

import java.text.MessageFormat;
import java.util.Date;

import li.strolch.exception.StrolchException;
import li.strolch.model.AbstractStrolchElement;
import li.strolch.model.Resource;
import li.strolch.model.StrolchValueType;
import li.strolch.model.Tags;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import ch.eitchnet.utils.dbc.DBC;
import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ResourceFromDomVisitor extends StrolchElementFromDomVisitor {

	public Resource visit(Document doc) {
		return fromDom(doc.getDocumentElement());
	}

	public Resource fromDom(Element resourceElement) {
		Resource resource = new Resource();
		fromDom(resourceElement, resource);

		NodeList timedStateElems = resourceElement.getElementsByTagName(Tags.TIMED_STATE);
		for (int i = 0; i < timedStateElems.getLength(); i++) {
			Element timedStateElem = (Element) timedStateElems.item(i);
			String typeS = timedStateElem.getAttribute(Tags.TYPE);

			DBC.PRE.assertNotEmpty("Type must be set on TimedState for resource with id " + resource.getId(), typeS);
			StrolchValueType valueType = StrolchValueType.parse(typeS);
			StrolchTimedState<? extends IValue<?>> timedState = valueType.timedStateInstance();

			fromDom(timedStateElem, (AbstractStrolchElement) timedState);

			String interpretation = timedStateElem.getAttribute(Tags.INTERPRETATION);
			String hidden = timedStateElem.getAttribute(Tags.HIDDEN);
			String uom = timedStateElem.getAttribute(Tags.UOM);
			String index = timedStateElem.getAttribute(Tags.INDEX);

			timedState.setInterpretation(interpretation);
			timedState.setUom(uom);

			if (StringHelper.isEmpty(index)) {
				timedState.setIndex(0);
			} else {
				timedState.setIndex(Integer.valueOf(index));
			}

			if (StringHelper.isEmpty(hidden)) {
				timedState.setHidden(false);
			} else {
				if (hidden.equalsIgnoreCase(Boolean.TRUE.toString())) {
					timedState.setHidden(true);
				} else if (hidden.equalsIgnoreCase(Boolean.FALSE.toString())) {
					timedState.setHidden(false);
				} else {
					String msg = "Boolean string must be either {0} or {1}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, Boolean.TRUE.toString(), Boolean.FALSE.toString());
					throw new StrolchException(msg);
				}
			}

			NodeList timeValueElems = timedStateElem.getElementsByTagName(Tags.VALUE);
			for (int j = 0; j < timeValueElems.getLength(); j++) {
				Element timeValueElem = (Element) timeValueElems.item(j);
				String timeS = timeValueElem.getAttribute(Tags.TIME);
				Date date = ISO8601FormatFactory.getInstance().parseDate(timeS);
				long time = date.getTime();

				String valueS = timeValueElem.getAttribute(Tags.VALUE);
				timedState.setStateFromStringAt(time, valueS);
			}

			resource.addTimedState(timedState);
		}

		return resource;
	}
}
