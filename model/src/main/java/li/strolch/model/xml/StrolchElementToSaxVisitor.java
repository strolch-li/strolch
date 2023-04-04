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
package li.strolch.model.xml;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

import java.text.MessageFormat;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Set;
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
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchElementToSaxVisitor implements StrolchRootElementVisitor<Void> {

	protected ContentHandler contentHandler;

	public StrolchElementToSaxVisitor(ContentHandler contentHandler) {
		this.contentHandler = contentHandler;
	}

	@Override
	public Void visitActivity(Activity activity) {
		try {

			toSax(activity);

		} catch (Exception e) {
			String msg = "Failed to transform Activity {0} to XML due to {1}";
			msg = MessageFormat.format(msg, activity.getLocator(), e.getMessage());
			throw new RuntimeException(msg, e);
		}

		return null;
	}

	@Override
	public Void visitOrder(Order order) {
		try {

			toSax(order);

		} catch (SAXException e) {
			String msg = "Failed to transform Order {0} to XML due to {1}";
			msg = MessageFormat.format(msg, order.getLocator(), e.getMessage());
			throw new RuntimeException(msg, e);
		}

		return null;
	}

	@Override
	public Void visitResource(Resource resource) {
		try {

			toSax(resource);

		} catch (Exception e) {
			String msg = "Failed to transform Resource {0} to XML due to {1}";
			msg = MessageFormat.format(msg, resource.getLocator(), e.getMessage());
			throw new RuntimeException(msg, e);
		}

		return null;
	}

	protected void toSax(Resource resource) throws SAXException {
		this.contentHandler.startElement(null, null, Tags.RESOURCE, attributesFor(resource));

		toSax((StrolchRootElement) resource);

		Set<String> stateKeySet = resource.getTimedStateKeySet();
		for (String stateKey : stateKeySet) {
			StrolchTimedState<IValue<?>> state = resource.getTimedState(stateKey);
			this.contentHandler.startElement(null, null, Tags.TIMED_STATE, attributesFor(state));

			SortedSet<ITimeValue<IValue<?>>> values = state.getTimeEvolution().getValues();
			for (ITimeValue<IValue<?>> value : values) {
				this.contentHandler.startElement(null, null, Tags.VALUE, attributesFor(value));
				this.contentHandler.endElement(null, null, Tags.VALUE);
			}
			this.contentHandler.endElement(null, null, Tags.TIMED_STATE);
		}

		this.contentHandler.endElement(null, null, Tags.RESOURCE);
	}

	protected void toSax(Order order) throws SAXException {
		this.contentHandler.startElement(null, null, Tags.ORDER, attributesFor(order));

		toSax((StrolchRootElement) order);

		this.contentHandler.endElement(null, null, Tags.ORDER);
	}

	protected void toSax(Activity activity) throws SAXException {
		this.contentHandler.startElement(null, null, Tags.ACTIVITY, attributesFor(activity));

		toSax((StrolchRootElement) activity);

		Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement activityElement = iter.next().getValue();

			if (activityElement instanceof Activity) {
				toSax((Activity) activityElement);
			} else if (activityElement instanceof Action) {
				toSax((Action) activityElement);
			} else {
				throw new IllegalArgumentException("Unhandled element " + activityElement.getClass());
			}
		}

		this.contentHandler.endElement(null, null, Tags.ACTIVITY);
	}

	protected void toSax(Action action) throws SAXException {
		this.contentHandler.startElement(null, null, Tags.ACTION, attributesFor(action));

		toSax((GroupedParameterizedElement) action);

		if (action.hasPolicyDefs())
			toSax(action.getPolicyDefs());

		Iterator<IValueChange<? extends IValue<?>>> iter = action.changesIterator();
		while (iter.hasNext()) {
			IValueChange<? extends IValue<?>> valueChange = iter.next();
			this.contentHandler.startElement(null, null, Tags.VALUE_CHANGE, attributesFor(valueChange));
			this.contentHandler.endElement(null, null, Tags.VALUE_CHANGE);
		}

		this.contentHandler.endElement(null, null, Tags.ACTION);
	}

	protected void toSax(PolicyDefs policyDefs) throws SAXException {
		if (!policyDefs.hasPolicyDefs())
			return;

		this.contentHandler.startElement(null, null, Tags.POLICIES, null);
		for (String type : policyDefs.getPolicyTypes()) {
			PolicyDef policyDef = policyDefs.getPolicyDef(type);
			this.contentHandler.startElement(null, null, Tags.POLICY, attributesFor(policyDef));
			this.contentHandler.endElement(null, null, Tags.POLICY);
		}
		this.contentHandler.endElement(null, null, Tags.POLICIES);
	}

	protected void toSax(StrolchRootElement rootElement) throws SAXException {

		if (rootElement.hasPolicyDefs())
			toSax(rootElement.getPolicyDefs());

		if (rootElement.hasVersion())
			toSax(rootElement.getVersion());

		toSax((GroupedParameterizedElement) rootElement);
	}

	private void toSax(Version version) throws SAXException {
		this.contentHandler.startElement(null, null, Tags.VERSION, attributesFor(version));
		this.contentHandler.endElement(null, null, Tags.VERSION);
	}

	protected void toSax(GroupedParameterizedElement parameterizedElement) throws SAXException {

		Set<String> bagKeySet = parameterizedElement.getParameterBagKeySet();
		for (String bagKey : bagKeySet) {
			ParameterBag parameterBag = parameterizedElement.getParameterBag(bagKey);
			this.contentHandler.startElement(null, null, Tags.PARAMETER_BAG, attributesFor(parameterBag));

			Set<String> parameterKeySet = parameterBag.getParameterKeySet();
			for (String paramKey : parameterKeySet) {
				Parameter<?> parameter = parameterBag.getParameter(paramKey);
				this.contentHandler.startElement(null, null, Tags.PARAMETER, attributesFor(parameter));
				if (parameter.getValueType() == StrolchValueType.TEXT) {
					String valueAsString = parameter.getValueAsString();
					this.contentHandler.characters(valueAsString.toCharArray(), 0, valueAsString.length());
				}
				this.contentHandler.endElement(null, null, Tags.PARAMETER);
			}

			this.contentHandler.endElement(null, null, Tags.PARAMETER_BAG);
		}
	}

	protected Attributes attributesFor(StrolchTimedState<IValue<?>> state) {
		AttributesImpl attributes = attributesFor((StrolchElement) state);

		if (!UOM_NONE.equals(state.getUom()))
			attributes.addAttribute(null, null, Tags.UOM, Tags.CDATA, state.getUom());

		if (!INTERPRETATION_NONE.equals(state.getInterpretation()))
			attributes.addAttribute(null, null, Tags.INTERPRETATION, Tags.CDATA, state.getInterpretation());

		if (state.isHidden())
			attributes.addAttribute(null, null, Tags.HIDDEN, Tags.CDATA, Boolean.toString(state.isHidden()));

		if (state.getIndex() != 0)
			attributes.addAttribute(null, null, Tags.INDEX, Tags.CDATA, Integer.toString(state.getIndex()));

		return attributes;
	}

	protected AttributesImpl attributesFor(StrolchElement element) {
		AttributesImpl attributes = new AttributesImpl();

		attributes.addAttribute(null, null, Tags.ID, Tags.CDATA, element.getId());
		attributes.addAttribute(null, null, Tags.NAME, Tags.CDATA, element.getName());
		attributes.addAttribute(null, null, Tags.TYPE, Tags.CDATA, element.getType());

		return attributes;
	}

	protected AttributesImpl attributesFor(Order order) {
		AttributesImpl attributes = attributesFor((StrolchElement) order);

		if (!order.getState().isCreated())
			attributes.addAttribute(null, null, Tags.STATE, Tags.CDATA, order.getState().getName());

		attributes.addAttribute(null, null, Tags.DATE, Tags.CDATA,
				ISO8601FormatFactory.getInstance().formatDate(order.getDate()));

		return attributes;
	}

	protected AttributesImpl attributesFor(Activity activity) {
		AttributesImpl attributes = attributesFor((StrolchElement) activity);

		if (!activity.getState().isCreated())
			attributes.addAttribute(null, null, Tags.STATE, Tags.CDATA, activity.getState().getName());

		attributes.addAttribute(null, null, Tags.TIME_ORDERING, Tags.CDATA, activity.getTimeOrdering().getName());

		return attributes;
	}

	private Attributes attributesFor(Version version) {
		AttributesImpl attributes = new AttributesImpl();

		attributes.addAttribute(null, null, Tags.VERSION, Tags.CDATA, Integer.toString(version.getVersion()));
		attributes.addAttribute(null, null, Tags.CREATED_BY, Tags.CDATA, version.getCreatedBy());
		attributes.addAttribute(null, null, Tags.UPDATED_BY, Tags.CDATA, version.getUpdatedBy());
		attributes.addAttribute(null, null, Tags.CREATED, Tags.CDATA,
				ISO8601FormatFactory.getInstance().formatDate(version.getCreated()));
		attributes.addAttribute(null, null, Tags.UPDATED, Tags.CDATA,
				ISO8601FormatFactory.getInstance().formatDate(version.getUpdated()));
		attributes.addAttribute(null, null, Tags.DELETED, Tags.CDATA, Boolean.toString(version.isDeleted()));

		return attributes;
	}

	protected AttributesImpl attributesFor(Parameter<?> parameter) {
		AttributesImpl attributes = attributesFor((StrolchElement) parameter);

		if (parameter.getValueType() != StrolchValueType.TEXT)
			attributes.addAttribute(null, null, Tags.VALUE, Tags.CDATA, parameter.getValueAsString());

		if (!UOM_NONE.equals(parameter.getUom()))
			attributes.addAttribute(null, null, Tags.UOM, Tags.CDATA, parameter.getUom());

		if (!INTERPRETATION_NONE.equals(parameter.getInterpretation()))
			attributes.addAttribute(null, null, Tags.INTERPRETATION, Tags.CDATA, parameter.getInterpretation());

		if (parameter.isHidden())
			attributes.addAttribute(null, null, Tags.HIDDEN, Tags.CDATA, Boolean.toString(parameter.isHidden()));

		if (parameter.getIndex() != 0)
			attributes.addAttribute(null, null, Tags.INDEX, Tags.CDATA, Integer.toString(parameter.getIndex()));

		return attributes;
	}

	protected Attributes attributesFor(ITimeValue<IValue<?>> value) {
		AttributesImpl attributes = new AttributesImpl();

		ISO8601FormatFactory df = ISO8601FormatFactory.getInstance();
		attributes.addAttribute(null, null, Tags.TIME, Tags.CDATA, df.formatDate(value.getTime()));

		attributes.addAttribute(null, null, Tags.VALUE, Tags.CDATA, value.getValue().getValueAsString());

		return attributes;
	}

	protected AttributesImpl attributesFor(PolicyDef policyDef) {
		AttributesImpl attributes = new AttributesImpl();

		attributes.addAttribute(null, null, Tags.TYPE, Tags.CDATA, policyDef.getType());
		attributes.addAttribute(null, null, Tags.VALUE, Tags.CDATA, policyDef.getValueForXml());

		return attributes;
	}

	protected AttributesImpl attributesFor(Action action) {
		AttributesImpl attributes = attributesFor((StrolchElement) action);

		if (isNotEmpty(action.getResourceId()))
			attributes.addAttribute(null, null, Tags.RESOURCE_ID, Tags.CDATA, action.getResourceId());

		if (isNotEmpty(action.getResourceType()))
			attributes.addAttribute(null, null, Tags.RESOURCE_TYPE, Tags.CDATA, action.getResourceType());

		if (!action.getState().isCreated())
			attributes.addAttribute(null, null, Tags.STATE, Tags.CDATA, action.getState().getName());

		return attributes;
	}

	protected AttributesImpl attributesFor(IValueChange<? extends IValue<?>> valueChange) {
		AttributesImpl attributes = new AttributesImpl();

		if (isNotEmpty(valueChange.getStateId()))
			attributes.addAttribute(null, null, Tags.STATE_ID, Tags.CDATA, valueChange.getStateId());

		attributes.addAttribute(null, null, Tags.TIME, Tags.CDATA,
				ISO8601FormatFactory.getInstance().formatDate(valueChange.getTime()));

		attributes.addAttribute(null, null, Tags.VALUE, Tags.CDATA, valueChange.getValue().getValueAsString());

		attributes.addAttribute(null, null, Tags.TYPE, Tags.CDATA, valueChange.getValue().getType());

		return attributes;
	}
}
