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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.Version;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class StrolchElementToSaxWriterVisitor {

	protected XMLStreamWriter writer;

	public StrolchElementToSaxWriterVisitor(XMLStreamWriter writer) {
		this.writer = writer;
	}

	protected void writeElement(Resource resource) throws XMLStreamException {
		boolean empty = !resource.hasParameterBags() && !resource.hasTimedStates() && !resource.hasPolicyDefs();

		writeStartStrolchElement(Tags.RESOURCE, empty, resource);

		if (resource.hasVersion())
			writeVersion(resource);

		if (resource.hasParameterBags())
			writeParameterBags(resource);

		if (resource.hasTimedStates())
			writeTimedStates(resource);

		if (resource.hasPolicyDefs())
			writePolicyDefs(resource.getPolicyDefs());

		if (!empty)
			this.writer.writeEndElement();
	}

	protected void writeElement(Order order) throws XMLStreamException {
		boolean empty = !order.hasParameterBags() && !order.hasPolicyDefs();

		writeStartStrolchElement(Tags.ORDER, empty, order);
		this.writer.writeAttribute(Tags.DATE, ISO8601FormatFactory.getInstance().formatDate(order.getDate()));
		this.writer.writeAttribute(Tags.STATE, order.getState().getName());

		if (order.hasVersion())
			writeVersion(order);

		if (order.hasParameterBags())
			writeParameterBags(order);

		if (order.hasPolicyDefs())
			writePolicyDefs(order.getPolicyDefs());

		if (!empty)
			this.writer.writeEndElement();
	}

	protected void writeElement(Activity activity) throws XMLStreamException {
		boolean empty = activity.hasVersion() && !activity.hasParameterBags() && !activity.hasElements()
				&& !activity.hasPolicyDefs();

		writeStartStrolchElement(Tags.ACTIVITY, empty, activity);
		this.writer.writeAttribute(Tags.TIME_ORDERING, activity.getTimeOrdering().getName());
		
		if (activity.hasVersion())
			writeVersion(activity);

		if (activity.hasParameterBags())
			writeParameterBags(activity);

		if (activity.hasElements()) {
			Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
			while (iter.hasNext()) {
				IActivityElement element = iter.next().getValue();
				if (element instanceof Activity)
					writeElement((Activity) element);
				else if (element instanceof Action)
					writeElement((Action) element);
				else
					throw new IllegalArgumentException("Unhandled Element class " + element.getClass());
			}
		}

		if (activity.hasPolicyDefs())
			writePolicyDefs(activity.getPolicyDefs());

		if (!empty)
			this.writer.writeEndElement();
	}

	private void writeVersion(StrolchRootElement rootElement) throws XMLStreamException {
		this.writer.writeEmptyElement(Tags.VERSION);
		Version version = rootElement.getVersion();
		this.writer.writeAttribute(Tags.VERSION, Integer.toString(version.getVersion()));
		this.writer.writeAttribute(Tags.CREATED_BY, version.getCreatedBy());
		this.writer.writeAttribute(Tags.CREATED_AT,
				ISO8601FormatFactory.getInstance().formatDate(version.getCreatedAt()));
		this.writer.writeAttribute(Tags.DELETED, Boolean.toString(version.isDeleted()));
	}

	protected <T> void writeElement(Action action) throws XMLStreamException {
		boolean empty = !action.hasParameterBags() && !action.hasChanges() && !action.hasPolicyDefs();

		writeStartStrolchElement(Tags.ACTION, empty, action);
		this.writer.writeAttribute(Tags.STATE, action.getState().getName());
		this.writer.writeAttribute(Tags.RESOURCE_ID, action.getResourceId());
		this.writer.writeAttribute(Tags.RESOURCE_TYPE, action.getResourceType());

		if (action.hasParameterBags())
			writeParameterBags(action);

		if (action.hasChanges()) {
			for (IValueChange<? extends IValue<?>> change : action.getChanges()) {
				this.writer.writeEmptyElement(Tags.VALUE_CHANGE);
				this.writer.writeAttribute(Tags.STATE_ID, change.getStateId());
				this.writer.writeAttribute(Tags.TIME, ISO8601FormatFactory.getInstance().formatDate(change.getTime()));
				this.writer.writeAttribute(Tags.VALUE, change.getValue().getValueAsString());
				this.writer.writeAttribute(Tags.TYPE, change.getValue().getType());
			}
		}

		if (action.hasPolicyDefs())
			writePolicyDefs(action.getPolicyDefs());

		if (!empty)
			this.writer.writeEndElement();
	}

	protected void writePolicyDefs(PolicyDefs policyDefs) throws XMLStreamException {

		if (!policyDefs.hasPolicyDefs())
			return;

		this.writer.writeStartElement(Tags.POLICIES);
		for (String type : policyDefs.getPolicyTypes()) {
			PolicyDef policyDef = policyDefs.getPolicyDef(type);

			this.writer.writeEmptyElement(Tags.POLICY);
			this.writer.writeAttribute(Tags.TYPE, policyDef.getType());
			this.writer.writeAttribute(Tags.VALUE, policyDef.getValueForXml());
		}
		this.writer.writeEndElement();
	}

	protected void writeTimedStates(Resource resource) throws XMLStreamException {
		List<StrolchTimedState<IValue<?>>> timedStates = resource.getTimedStates();
		for (StrolchTimedState<IValue<?>> timedState : timedStates) {
			ITimeVariable<IValue<?>> timeEvolution = timedState.getTimeEvolution();
			SortedSet<ITimeValue<IValue<?>>> values = timeEvolution.getValues();

			writeStartStrolchElement(Tags.TIMED_STATE, values.isEmpty(), timedState);

			for (ITimeValue<IValue<?>> timeValue : values) {
				this.writer.writeEmptyElement(Tags.VALUE);
				this.writer.writeAttribute(Tags.TIME,
						ISO8601FormatFactory.getInstance().formatDate(timeValue.getTime()));
				this.writer.writeAttribute(Tags.VALUE, timeValue.getValue().getValueAsString());
			}

			if (!values.isEmpty())
				this.writer.writeEndElement();
		}
	}

	protected void writeStartStrolchElement(String tag, boolean empty, StrolchElement element)
			throws XMLStreamException {
		if (empty)
			this.writer.writeEmptyElement(tag);
		else
			this.writer.writeStartElement(tag);

		this.writer.writeAttribute(Tags.ID, element.getId());
		this.writer.writeAttribute(Tags.NAME, element.getName());
		this.writer.writeAttribute(Tags.TYPE, element.getType());
	}

	protected void writeParameterBags(GroupedParameterizedElement element) throws XMLStreamException {
		Set<String> bagKeySet = new TreeSet<>(element.getParameterBagKeySet());
		for (String bagKey : bagKeySet) {
			ParameterBag parameterBag = element.getParameterBag(bagKey);
			boolean isEmpty = !parameterBag.hasParameters();
			writeStartStrolchElement(Tags.PARAMETER_BAG, isEmpty, parameterBag);
			if (!isEmpty) {
				writeParameters(parameterBag);
				this.writer.writeEndElement();
			}
		}
	}

	protected void writeParameters(ParameterizedElement element) throws XMLStreamException {

		List<Parameter<?>> parameters = new ArrayList<>(element.getParameters());
		Collections.sort(parameters, (o1, o2) -> Integer.valueOf(o1.getIndex()).compareTo(o2.getIndex()));
		for (Parameter<?> parameter : parameters) {
			writeStartStrolchElement(Tags.PARAMETER, true, parameter);

			if (!INTERPRETATION_NONE.equals(parameter.getInterpretation()))
				this.writer.writeAttribute(Tags.INTERPRETATION, parameter.getInterpretation());
			if (!UOM_NONE.equals(parameter.getUom()))
				this.writer.writeAttribute(Tags.UOM, parameter.getUom());
			if (parameter.isHidden())
				this.writer.writeAttribute(Tags.HIDDEN, Boolean.toString(parameter.isHidden()));
			if (parameter.getIndex() != 0)
				this.writer.writeAttribute(Tags.INDEX, Integer.toString(parameter.getIndex()));

			this.writer.writeAttribute(Tags.VALUE, parameter.getValueAsString());
		}
	}
}
