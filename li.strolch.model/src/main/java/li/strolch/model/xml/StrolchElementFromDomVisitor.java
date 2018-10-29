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
import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchElementFromDomVisitor {

	public void fillElement(Element element, Order order) {
		fillElement(element, (StrolchRootElement) order);

		String date = element.getAttribute(Tags.DATE);
		String state = element.getAttribute(Tags.STATE);

		if (StringHelper.isEmpty(date)) {
			order.setDate(ISO8601FormatFactory.getInstance().getDateFormat().parse("-")); //$NON-NLS-1$
		} else {
			order.setDate(ISO8601FormatFactory.getInstance().getDateFormat().parse(date));
		}

		if (state == null || state.isEmpty()) {
			order.setState(State.CREATED);
		} else {
			order.setState(State.parse(state));
		}
	}

	public void fillElement(Element resourceElement, Resource resource) {
		fillElement(resourceElement, (StrolchRootElement) resource);

		NodeList childNodes = resourceElement.getChildNodes();
		for (int i = 0; i < childNodes.getLength(); i++) {
			Node item = childNodes.item(i);
			if (!(item instanceof Element))
				continue;

			Element timedStateElem = (Element) item;
			if (!timedStateElem.getNodeName().equals(Tags.TIMED_STATE))
				continue;

			String typeS = timedStateElem.getAttribute(Tags.TYPE);

			DBC.PRE.assertNotEmpty("Type must be set on TimedState for resource with id " + resource.getId(), typeS);
			StrolchValueType valueType = StrolchValueType.parse(typeS);
			StrolchTimedState<? extends IValue<?>> timedState = valueType.timedStateInstance();

			fillElement(timedStateElem, (AbstractStrolchElement) timedState);

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

			NodeList timeValueElems = timedStateElem.getChildNodes();
			for (int j = 0; j < timeValueElems.getLength(); j++) {
				Node timeValueItem = timeValueElems.item(j);
				if (!(timeValueItem instanceof Element))
					continue;

				Element timeValueElem = (Element) timeValueItem;
				if (!timeValueElem.getNodeName().equals(Tags.VALUE))
					continue;

				String timeS = timeValueElem.getAttribute(Tags.TIME);
				Date date = ISO8601FormatFactory.getInstance().parseDate(timeS);
				long time = date.getTime();

				String valueS = timeValueElem.getAttribute(Tags.VALUE);
				timedState.setStateFromStringAt(time, valueS);
			}

			resource.addTimedState(timedState);
		}
	}

	public void fillElement(Element activityElement, Activity activity) {
		fillElement(activityElement, (StrolchRootElement) activity);

		String timeOrderingS = activityElement.getAttribute(Tags.TIME_ORDERING);
		if (StringHelper.isEmpty(timeOrderingS))
			throw new StrolchException("TimeOrdering is not set for " + activity.getLocator());
		TimeOrdering timeOrdering = TimeOrdering.parse(timeOrderingS);
		activity.setTimeOrdering(timeOrdering);

		NodeList childNodes = activityElement.getChildNodes();
		for (int i = 0; i < childNodes.getLength(); i++) {
			Node item = childNodes.item(i);
			if (!(item instanceof Element))
				continue;

			Element childElem = (Element) item;

			switch (childElem.getNodeName()) {
			case Tags.ACTIVITY:
				Activity childActivity = new Activity();
				fillElement(childElem, childActivity);
				activity.addElement(childActivity);
				break;
			case Tags.ACTION:
				Action childAction = new Action();
				fillElement(childElem, childAction);
				activity.addElement(childAction);
				break;
			case Tags.PARAMETER_BAG:
			case Tags.POLICIES:
			case Tags.VERSION:
				break;
			default:
				throw new IllegalArgumentException("Unexpected element tag " + childElem.getNodeName());
			}
		}
	}

	protected void fillElement(Element element, AbstractStrolchElement strolchElement) {
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

	protected void fillElement(Element element, GroupedParameterizedElement groupedParameterizedElement) {
		fillElement(element, (AbstractStrolchElement) groupedParameterizedElement);

		String type = element.getAttribute(Tags.TYPE);
		groupedParameterizedElement.setType(type);

		NodeList bags = element.getChildNodes();
		for (int i = 0; i < bags.getLength(); i++) {
			Node item = bags.item(i);
			if (!(item instanceof Element))
				continue;

			Element bagElement = (Element) item;
			if (!bagElement.getNodeName().equals(Tags.PARAMETER_BAG))
				continue;

			ParameterBag bag = new ParameterBag();
			fillElement(bagElement, bag);
			groupedParameterizedElement.addParameterBag(bag);
		}
	}

	protected void fillElement(Element element, StrolchRootElement strolchRootElement) {
		fillElement(element, (GroupedParameterizedElement) strolchRootElement);

		parseVersion(strolchRootElement, element);

		PolicyDefs defs = parsePolicies(element);
		if (defs.hasPolicyDefs())
			strolchRootElement.setPolicyDefs(defs);
	}

	protected void fillElement(Element element, ParameterizedElement parameterizedElement) {
		fillElement(element, (AbstractStrolchElement) parameterizedElement);

		String type = element.getAttribute(Tags.TYPE);
		parameterizedElement.setType(type);

		// add all the parameters
		NodeList parameterElements = element.getChildNodes();
		for (int i = 0; i < parameterElements.getLength(); i++) {
			Node item = parameterElements.item(i);
			if (!(item instanceof Element))
				continue;

			Element paramElement = (Element) item;
			if (!paramElement.getNodeName().equals(Tags.PARAMETER))
				continue;

			String paramtype = paramElement.getAttribute(Tags.TYPE);

			StrolchValueType paramValueType = StrolchValueType.parse(paramtype);
			Parameter<?> parameter = paramValueType.parameterInstance();
			fillElement(paramElement, parameter);
			parameterizedElement.addParameter(parameter);
		}
	}

	protected void fillElement(Element element, Parameter<?> param) {

		fillElement(element, (AbstractStrolchElement) param);

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

	protected void fillElement(Element element, Action action) {
		fillElement(element, (GroupedParameterizedElement) action);

		String resourceId = element.getAttribute(Tags.RESOURCE_ID);
		String resourceType = element.getAttribute(Tags.RESOURCE_TYPE);
		String stateS = element.getAttribute(Tags.STATE);

		action.setResourceId(resourceId);
		action.setResourceType(resourceType);
		action.setState(State.parse(stateS));

		PolicyDefs defs = parsePolicies(element);
		if (defs.hasPolicyDefs())
			action.setPolicyDefs(defs);

		NodeList valueChangeNodes = element.getChildNodes();
		for (int i = 0; i < valueChangeNodes.getLength(); i++) {
			Node item = valueChangeNodes.item(i);
			if (!(item instanceof Element))
				continue;

			Element valueChangeElem = (Element) item;
			if (!valueChangeElem.getNodeName().equals(Tags.VALUE_CHANGE))
				continue;

			String stateId = valueChangeElem.getAttribute(Tags.STATE_ID);
			String timeS = valueChangeElem.getAttribute(Tags.TIME);
			String valueS = valueChangeElem.getAttribute(Tags.VALUE);
			String typeS = valueChangeElem.getAttribute(Tags.TYPE);

			StrolchValueType type = StrolchValueType.parse(typeS);
			IValue<?> value = type.valueInstance(valueS);

			long time = ISO8601FormatFactory.getInstance().getDateFormat().parse(timeS).getTime();
			ValueChange<IValue<?>> valueChange = new ValueChange<>(time, value, stateId);

			action.addChange(valueChange);
		}
	}

	protected PolicyDefs parsePolicies(Element element) {

		PolicyDefs policyDefs = new PolicyDefs();

		NodeList childNodes = element.getChildNodes();
		for (int i = 0; i < childNodes.getLength(); i++) {
			Node resourceChildItem = childNodes.item(i);
			if (!(resourceChildItem instanceof Element))
				continue;

			Element policiesElem = (Element) resourceChildItem;
			if (!policiesElem.getNodeName().equals(Tags.POLICIES))
				continue;

			NodeList policyChildNodes = policiesElem.getChildNodes();
			for (int j = 0; j < policyChildNodes.getLength(); j++) {
				Node policiesChildItem = policyChildNodes.item(j);
				if (!(policiesChildItem instanceof Element))
					continue;

				Element policyElem = (Element) policiesChildItem;
				if (!policyElem.getNodeName().equals(Tags.POLICY))
					continue;

				String type = policyElem.getAttribute(Tags.TYPE);
				String value = policyElem.getAttribute(Tags.VALUE);

				policyDefs.addOrUpdate(PolicyDef.valueOf(type, value));
			}
		}

		return policyDefs;
	}

	protected void parseVersion(StrolchRootElement rootElement, Element element) {

		NodeList childNodes = element.getChildNodes();
		for (int i = 0; i < childNodes.getLength(); i++) {
			Node resourceChildItem = childNodes.item(i);
			if (!(resourceChildItem instanceof Element))
				continue;

			Element versionElem = (Element) resourceChildItem;
			if (!versionElem.getNodeName().equals(Tags.VERSION))
				continue;

			int v = Integer.parseInt(versionElem.getAttribute(Tags.VERSION));
			String createdBy = versionElem.getAttribute(Tags.CREATED_BY);

			String createdS;
			if (versionElem.hasAttribute("CreatedAt"))
				createdS = versionElem.getAttribute("CreatedAt");
			else
				createdS = versionElem.getAttribute(Tags.CREATED);
			Date created = ISO8601FormatFactory.getInstance().parseDate(createdS);

			Date updated;
			if (versionElem.hasAttribute(Tags.UPDATED)) {
				String updatedS = versionElem.getAttribute(Tags.UPDATED);
				updated = ISO8601FormatFactory.getInstance().parseDate(updatedS);
			} else {
				updated = created;
			}

			boolean deleted = StringHelper.parseBoolean(versionElem.getAttribute(Tags.DELETED));
			Version version = new Version(rootElement.getLocator(), v, createdBy, created, updated, deleted);

			rootElement.setVersion(version);
		}
	}
}
