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
import java.util.Comparator;
import java.util.List;
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
import li.strolch.model.Tags;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractToSaxWriterVisitor {

	protected XMLStreamWriter writer;

	public AbstractToSaxWriterVisitor(XMLStreamWriter writer) {
		this.writer = writer;
	}

	protected void writeElement(String tag, Order order) throws XMLStreamException {
		boolean empty = !order.hasParameterBags();
		writeElement(tag, empty, order);
		if (!empty)
			this.writer.writeEndElement();
	}

	protected void writeElement(String tag, Resource resource) throws XMLStreamException {
		boolean empty = !resource.hasParameterBags() && !resource.hasTimedStates();
		writeElement(tag, empty, resource);

		if (resource.hasTimedStates())
			writeTimedStates(resource);

		if (!empty)
			this.writer.writeEndElement();
	}

	/**
	 * @param resource
	 * @throws XMLStreamException
	 */
	private void writeTimedStates(Resource resource) throws XMLStreamException {
		List<StrolchTimedState<IValue<?>>> timedStates = resource.getTimedStates();
		for (StrolchTimedState<IValue<?>> timedState : timedStates) {
			ITimeVariable<IValue<?>> timeEvolution = timedState.getTimeEvolution();
			SortedSet<ITimeValue<IValue<?>>> values = timeEvolution.getValues();

			writeStartStrolchElement(Tags.TIMED_STATE, values.isEmpty(), timedState);

			for (ITimeValue<IValue<?>> timeValue : values) {
				this.writer.writeEmptyElement(Tags.VALUE);
				this.writer.writeAttribute(Tags.TIME, timeValue.getTime().toString());
				this.writer.writeAttribute(Tags.VALUE, timeValue.getValue().getValueAsString());
			}

			if (!values.isEmpty())
				this.writer.writeEndElement();
		}
	}

	protected void writeElement(String tag, boolean empty, GroupedParameterizedElement element)
			throws XMLStreamException {
		writeStartStrolchElement(tag, empty, element);
		if (!empty) {
			writeParameterBags(element);
		}
	}

	protected void writeStartStrolchElement(String tag, boolean empty, StrolchElement element)
			throws XMLStreamException {
		if (empty) {
			this.writer.writeEmptyElement(tag);
		} else {
			this.writer.writeStartElement(tag);
		}

		this.writer.writeAttribute(Tags.ID, element.getId());
		if (StringHelper.isNotEmpty(element.getName())) {
			this.writer.writeAttribute(Tags.NAME, element.getName());
		}
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
		Collections.sort(parameters, new Comparator<Parameter<?>>() {
			@Override
			public int compare(Parameter<?> o1, Parameter<?> o2) {
				return Integer.valueOf(o1.getIndex()).compareTo(o2.getIndex());
			}
		});
		for (Parameter<?> parameter : parameters) {
			writeStartStrolchElement(Tags.PARAMETER, true, parameter);

			if (!INTERPRETATION_NONE.equals(parameter.getInterpretation())) {
				this.writer.writeAttribute(Tags.INTERPRETATION, parameter.getInterpretation());
			}
			if (!UOM_NONE.equals(parameter.getUom())) {
				this.writer.writeAttribute(Tags.UOM, parameter.getUom());
			}
			if (parameter.isHidden()) {
				this.writer.writeAttribute(Tags.HIDDEN, Boolean.toString(parameter.isHidden()));
			}
			if (parameter.getIndex() != 0) {
				this.writer.writeAttribute(Tags.INDEX, Integer.toString(parameter.getIndex()));
			}

			this.writer.writeAttribute(Tags.VALUE, parameter.getValueAsString());
		}
	}
}
