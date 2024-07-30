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

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

import javax.xml.parsers.DocumentBuilder;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.SortedSet;

import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import li.strolch.utils.helper.DomUtil;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchElementToDomVisitor implements StrolchRootElementVisitor<Document> {

	protected Document document;

	public Document getDocument() {
		return this.document;
	}

	@Override
	public Document visitActivity(Activity activity) {
		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		this.document = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element asDom = toDom(activity);
		document.appendChild(asDom);
		return this.document;
	}

	@Override
	public Document visitOrder(Order order) {
		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		this.document = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element asDom = toDom(order);
		document.appendChild(asDom);
		return this.document;
	}

	@Override
	public Document visitResource(Resource resource) {
		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		this.document = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element asDom = toDom(resource);
		document.appendChild(asDom);
		return this.document;
	}

	public Element toDom(Order order) {

		Element asDom = document.createElement(Tags.ORDER);
		asDom.setAttribute(Tags.DATE, ISO8601FormatFactory.getInstance().formatDate(order.getDate()));
		asDom.setAttribute(Tags.STATE, order.getState().getName());

		fillElement(asDom, (StrolchRootElement) order);

		return asDom;
	}

	public Element toDom(Resource resource) {

		Element asDom = document.createElement(Tags.RESOURCE);
		fillElement(asDom, (StrolchRootElement) resource);

		if (resource.hasTimedStates()) {
			for (String stateKey : resource.getTimedStateKeySet()) {
				StrolchTimedState<IValue<?>> timedState = resource.getTimedState(stateKey);
				Element stateElement = toDom(timedState);
				asDom.appendChild(stateElement);
			}
		}

		return asDom;
	}

	public Element toDom(Activity activity) {
		Element element = document.createElement(Tags.ACTIVITY);
		element.setAttribute(Tags.TIME_ORDERING, activity.getTimeOrdering().getName());

		fillElement(element, (StrolchRootElement) activity);

		if (activity.hasElements()) {
			Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
			while (iter.hasNext()) {
				IActivityElement activityElement = iter.next().getValue();
				if (activityElement instanceof Activity)
					element.appendChild(toDom((Activity) activityElement));
				else if (activityElement instanceof Action)
					element.appendChild(toDom((Action) activityElement));
				else
					throw new IllegalArgumentException("Unhandled element " + activityElement.getClass());
			}
		}

		return element;
	}

	public Element toDom(Action action) {
		Element element = document.createElement(Tags.ACTION);
		fillElement(element, action);

		if (isNotEmpty(action.getResourceId()))
			element.setAttribute(Tags.RESOURCE_ID, action.getResourceId());
		if (isNotEmpty(action.getResourceType()))
			element.setAttribute(Tags.RESOURCE_TYPE, action.getResourceType());

		element.setAttribute(Tags.STATE, action.getState().getName());

		if (action.hasPolicyDefs())
			fillElement(element, action.getPolicyDefs());

		if (action.hasChanges()) {
			Iterator<IValueChange<? extends IValue<?>>> iter = action.changesIterator();
			while (iter.hasNext()) {
				IValueChange<? extends IValue<?>> value = iter.next();
				Element valueChangeElement = toDom(value);
				element.appendChild(valueChangeElement);
			}
		}

		return element;
	}

	protected Element toDom(IValueChange<? extends IValue<?>> value) {
		Element element = document.createElement(Tags.VALUE_CHANGE);
		if (isNotEmpty(value.getStateId()))
			element.setAttribute(Tags.STATE_ID, value.getStateId());
		element.setAttribute(Tags.TIME, ISO8601FormatFactory.getInstance().formatDate(value.getTime()));
		element.setAttribute(Tags.VALUE, value.getValue().getValueAsString());
		element.setAttribute(Tags.TYPE, value.getValue().getType());
		return element;
	}

	protected Element toDom(StrolchTimedState<IValue<?>> timedState) {

		Element element = document.createElement(Tags.TIMED_STATE);
		fillElement(element, (AbstractStrolchElement) timedState);

		if (!timedState.getInterpretation().equals(INTERPRETATION_NONE))
			element.setAttribute(Tags.INTERPRETATION, timedState.getInterpretation());
		if (!timedState.getUom().equals(UOM_NONE))
			element.setAttribute(Tags.UOM, timedState.getUom());
		if (timedState.isHidden()) {
			element.setAttribute(Tags.HIDDEN, Boolean.toString(timedState.isHidden()));
		}
		if (timedState.getIndex() != 0) {
			element.setAttribute(Tags.INDEX, Integer.toString(timedState.getIndex()));
		}

		SortedSet<ITimeValue<IValue<?>>> values = timedState.getTimeEvolution().getValues();
		for (ITimeValue<IValue<?>> iTimeValue : values) {

			Long time = iTimeValue.getTime();
			String valueS = iTimeValue.getValue().getValueAsString();

			Element valueElement = document.createElement(Tags.VALUE);
			valueElement.setAttribute(Tags.TIME, ISO8601FormatFactory.getInstance().formatDate(time));
			valueElement.setAttribute(Tags.VALUE, valueS);

			element.appendChild(valueElement);
		}

		return element;
	}

	protected Element toDom(ParameterBag bag) {
		Element bagElement = document.createElement(Tags.PARAMETER_BAG);
		fillElement(bagElement, bag);
		return bagElement;
	}

	protected Element toDom(Parameter<?> param) {
		Element element = document.createElement(Tags.PARAMETER);
		fillElement(element, (AbstractStrolchElement) param);

		if (!param.getInterpretation().equals(INTERPRETATION_NONE))
			element.setAttribute(Tags.INTERPRETATION, param.getInterpretation());
		if (!param.getUom().equals(UOM_NONE))
			element.setAttribute(Tags.UOM, param.getUom());
		if (param.isHidden())
			element.setAttribute(Tags.HIDDEN, Boolean.toString(param.isHidden()));
		if (param.getIndex() != 0)
			element.setAttribute(Tags.INDEX, Integer.toString(param.getIndex()));

		if (param.getValueType() == StrolchValueType.TEXT)
			element.setTextContent(param.getValueAsString());
		else
			element.setAttribute(Tags.VALUE, param.getValueAsString());

		return element;
	}

	protected Element toDom(Version version) {
		Element element = document.createElement(Tags.VERSION);
		element.setAttribute(Tags.VERSION, Integer.toString(version.getVersion()));
		element.setAttribute(Tags.CREATED_BY, version.getCreatedBy());
		element.setAttribute(Tags.UPDATED_BY, version.getUpdatedBy());
		element.setAttribute(Tags.CREATED, ISO8601FormatFactory.getInstance().formatDate(version.getCreated()));
		element.setAttribute(Tags.UPDATED, ISO8601FormatFactory.getInstance().formatDate(version.getUpdated()));
		element.setAttribute(Tags.DELETED, Boolean.toString(version.isDeleted()));
		return element;
	}

	protected void fillElement(Element element, AbstractStrolchElement strolchElement) {
		element.setAttribute(Tags.ID, strolchElement.getId());
		element.setAttribute(Tags.NAME, strolchElement.getName());
		element.setAttribute(Tags.TYPE, strolchElement.getType());
	}

	protected void fillElement(Element element, StrolchRootElement rootElement) {
		fillElement(element, (GroupedParameterizedElement) rootElement);

		if (rootElement.hasVersion())
			element.appendChild(toDom(rootElement.getVersion()));

		if (rootElement.hasPolicyDefs())
			fillElement(element, rootElement.getPolicyDefs());
	}

	protected void fillElement(Element element, GroupedParameterizedElement rootElement) {
		fillElement(element, (AbstractStrolchElement) rootElement);

		if (rootElement.hasParameterBags()) {
			for (String bagKey : rootElement.getParameterBagKeySet()) {
				ParameterBag bag = rootElement.getParameterBag(bagKey);
				Element bagElement = toDom(bag);
				element.appendChild(bagElement);
			}
		}
	}

	protected void fillElement(Element element, ParameterizedElement parameterizedElement) {
		fillElement(element, (AbstractStrolchElement) parameterizedElement);

		if (parameterizedElement.hasParameters()) {
			for (String paramKey : parameterizedElement.getParameterKeySet()) {
				Parameter<?> parameter = parameterizedElement.getParameter(paramKey);
				Element paramElement = toDom(parameter);
				element.appendChild(paramElement);
			}
		}
	}

	protected void fillElement(Element asDom, PolicyDefs policyDefs) {
		if (policyDefs == null || !policyDefs.hasPolicyDefs())
			return;

		Element policiesElem = this.document.createElement(Tags.POLICIES);

		for (String type : policyDefs.getPolicyTypes()) {
			PolicyDef policyDef = policyDefs.getPolicyDef(type);

			Element policyElem = this.document.createElement(Tags.POLICY);
			policyElem.setAttribute(Tags.TYPE, policyDef.getType());
			policyElem.setAttribute(Tags.VALUE, policyDef.getValueForXml());

			policiesElem.appendChild(policyElem);
		}

		asDom.appendChild(policiesElem);
	}
}
