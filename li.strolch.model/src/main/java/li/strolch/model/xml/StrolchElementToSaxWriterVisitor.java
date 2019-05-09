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

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.text.MessageFormat;
import java.util.*;
import java.util.Map.Entry;

import li.strolch.exception.StrolchException;
import li.strolch.model.*;
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
import li.strolch.model.visitor.StrolchRootElementVisitor;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchElementToSaxWriterVisitor implements StrolchRootElementVisitor<Void> {

	protected XMLStreamWriter writer;

	public StrolchElementToSaxWriterVisitor(XMLStreamWriter writer) {
		this.writer = writer;
	}

	@Override
	public Void visitActivity(Activity activity) {

		try {
			writeElement(activity);
			this.writer.flush();
		} catch (XMLStreamException e) {
			String msg = "Failed to write Activity {0} due to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, activity.getLocator(), e.getMessage());
			throw new StrolchException(msg, e);
		}

		return null;
	}

	@Override
	public Void visitOrder(Order order) {

		try {
			writeElement(order);
			this.writer.flush();
		} catch (XMLStreamException e) {
			String msg = "Failed to write Order {0} due to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, order.getLocator(), e.getMessage());
			throw new StrolchException(msg, e);
		}

		return null;
	}

	@Override
	public Void visitResource(Resource resource) {

		try {
			writeElement(resource);
			this.writer.flush();
		} catch (XMLStreamException e) {
			String msg = "Failed to write Resource {0} due to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, resource.getLocator(), e.getMessage());
			throw new StrolchException(msg, e);
		}

		return null;
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
		boolean empty = activity.hasVersion() && !activity.hasParameterBags() && !activity.hasElements() && !activity
				.hasPolicyDefs();

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
		this.writer.writeAttribute(Tags.UPDATED_BY, version.getUpdatedBy());
		this.writer.writeAttribute(Tags.CREATED, ISO8601FormatFactory.getInstance().formatDate(version.getCreated()));
		this.writer.writeAttribute(Tags.UPDATED, ISO8601FormatFactory.getInstance().formatDate(version.getUpdated()));
		this.writer.writeAttribute(Tags.DELETED, Boolean.toString(version.isDeleted()));
	}

	protected <T> void writeElement(Action action) throws XMLStreamException {
		boolean empty = !action.hasParameterBags() && !action.hasChanges() && !action.hasPolicyDefs();

		writeStartStrolchElement(Tags.ACTION, empty, action);
		this.writer.writeAttribute(Tags.STATE, action.getState().getName());
		if (isNotEmpty(action.getResourceId()))
			this.writer.writeAttribute(Tags.RESOURCE_ID, action.getResourceId());
		if (isNotEmpty(action.getResourceType()))
			this.writer.writeAttribute(Tags.RESOURCE_TYPE, action.getResourceType());

		if (action.hasParameterBags())
			writeParameterBags(action);

		if (action.hasChanges()) {
			for (IValueChange<? extends IValue<?>> change : action.getChanges()) {
				this.writer.writeEmptyElement(Tags.VALUE_CHANGE);
				if (isNotEmpty(change.getStateId()))
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

			if (!INTERPRETATION_NONE.equals(timedState.getInterpretation()))
				this.writer.writeAttribute(Tags.INTERPRETATION, timedState.getInterpretation());
			if (!UOM_NONE.equals(timedState.getUom()))
				this.writer.writeAttribute(Tags.UOM, timedState.getUom());
			if (timedState.isHidden())
				this.writer.writeAttribute(Tags.HIDDEN, Boolean.toString(timedState.isHidden()));
			if (timedState.getIndex() != 0)
				this.writer.writeAttribute(Tags.INDEX, Integer.toString(timedState.getIndex()));

			for (ITimeValue<IValue<?>> timeValue : values) {
				this.writer.writeEmptyElement(Tags.VALUE);
				this.writer
						.writeAttribute(Tags.TIME, ISO8601FormatFactory.getInstance().formatDate(timeValue.getTime()));
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
		parameters.sort(Comparator.comparingInt(Parameter::getIndex));
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
