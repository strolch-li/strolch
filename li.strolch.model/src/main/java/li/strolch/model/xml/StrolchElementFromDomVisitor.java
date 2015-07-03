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

import li.strolch.exception.StrolchException;
import li.strolch.model.AbstractStrolchElement;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.ParameterBag;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.StrolchValueType;
import li.strolch.model.Tags;
import li.strolch.model.parameter.Parameter;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchElementFromDomVisitor {

	protected void fromDom(Element element, AbstractStrolchElement strolchElement) {
		String id = element.getAttribute(Tags.ID);
		String name = element.getAttribute(Tags.NAME);

		if (id != null && name != null) {
			strolchElement.setId(id);
			strolchElement.setName(name);
		} else {
			String msg = "Check the values of the element: {0} either id or name attribute is null!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getNodeName());
			throw new StrolchException(msg);
		}
	}

	protected void fromDom(Element element, GroupedParameterizedElement order) {
		fromDom(element, (AbstractStrolchElement) order);

		String type = element.getAttribute(Tags.TYPE);
		order.setType(type);

		NodeList bags = element.getElementsByTagName(Tags.PARAMETER_BAG);
		for (int i = 0; i < bags.getLength(); i++) {
			Element bagElement = (Element) bags.item(i);
			ParameterBag bag = new ParameterBag();
			fromDom(bagElement, bag);
			order.addParameterBag(bag);
		}
	}

	protected void fromDom(Element element, ParameterizedElement parameterizedElement) {
		fromDom(element, (AbstractStrolchElement) parameterizedElement);

		String type = element.getAttribute(Tags.TYPE);
		parameterizedElement.setType(type);

		// add all the parameters
		NodeList parameterElements = element.getElementsByTagName(Tags.PARAMETER);
		for (int i = 0; i < parameterElements.getLength(); i++) {
			Element paramElement = (Element) parameterElements.item(i);
			String paramtype = paramElement.getAttribute(Tags.TYPE);

			StrolchValueType paramValueType = StrolchValueType.parse(paramtype);
			Parameter<?> parameter = paramValueType.parameterInstance();
			fromDom(paramElement, parameter);
			parameterizedElement.addParameter(parameter);
		}
	}

	protected void fromDom(Element element, Parameter<?> param) {

		fromDom(element, (AbstractStrolchElement) param);

		String value = element.getAttribute(Tags.VALUE);
		param.setValueFromString(value);

		String interpretation = element.getAttribute(Tags.INTERPRETATION);
		String hidden = element.getAttribute(Tags.HIDDEN);
		String uom = element.getAttribute(Tags.UOM);
		String index = element.getAttribute(Tags.INDEX);

		param.setInterpretation(interpretation);
		param.setUom(uom);

		if (StringHelper.isEmpty(index)) {
			param.setIndex(0);
		} else {
			param.setIndex(Integer.valueOf(index));
		}

		if (StringHelper.isEmpty(hidden)) {
			param.setHidden(false);
		} else {
			if (hidden.equalsIgnoreCase(Boolean.TRUE.toString())) {
				param.setHidden(true);
			} else if (hidden.equalsIgnoreCase(Boolean.FALSE.toString())) {
				param.setHidden(false);
			} else {
				String msg = "Boolean string must be either {0} or {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, Boolean.TRUE.toString(), Boolean.FALSE.toString());
				throw new StrolchException(msg);
			}
		}
	}
}
