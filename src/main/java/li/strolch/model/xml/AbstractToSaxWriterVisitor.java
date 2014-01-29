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

import java.util.Set;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.ParameterBag;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.StrolchElement;
import li.strolch.model.Tags;
import li.strolch.model.parameter.Parameter;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractToSaxWriterVisitor {

	protected XMLStreamWriter writer;

	public AbstractToSaxWriterVisitor(XMLStreamWriter writer) {
		this.writer = writer;
	}

	protected void writeElement(String tag, GroupedParameterizedElement element) throws XMLStreamException {
		boolean isEmpty = !element.hasParameterBags();
		writeStartStrolchElement(tag, isEmpty, element);
		if (!isEmpty) {
			writeParameterBags(element);
			writer.writeEndElement();
		}
	}

	protected void writeStartStrolchElement(String tag, boolean empty, StrolchElement element)
			throws XMLStreamException {
		if (empty)
			writer.writeEmptyElement(tag);
		else
			writer.writeStartElement(tag);

		writer.writeAttribute(Tags.ID, element.getId());
		if (!StringHelper.isEmpty(element.getName()))
			writer.writeAttribute(Tags.NAME, element.getName());
		writer.writeAttribute(Tags.TYPE, element.getType());
	}

	protected void writeParameters(ParameterizedElement element) throws XMLStreamException {

		Set<String> parameterKeySet = element.getParameterKeySet();
		for (String paramKey : parameterKeySet) {
			Parameter<?> parameter = element.getParameter(paramKey);

			writeStartStrolchElement(Tags.PARAMETER, true, parameter);

			if (!Parameter.INTERPRETATION_NONE.equals(parameter.getInterpretation()))
				writer.writeAttribute(Tags.INTERPRETATION, parameter.getInterpretation());
			if (!Parameter.UOM_NONE.equals(parameter.getUom()))
				writer.writeAttribute(Tags.UOM, parameter.getUom());
			if (parameter.isHidden())
				writer.writeAttribute(Tags.HIDDEN, Boolean.toString(parameter.isHidden()));

			writer.writeAttribute(Tags.VALUE, parameter.getValueAsString());
		}
	}

	protected void writeParameterBags(GroupedParameterizedElement element) throws XMLStreamException {

		Set<String> bagKeySet = element.getParameterBagKeySet();
		for (String bagKey : bagKeySet) {
			ParameterBag parameterBag = element.getParameterBag(bagKey);
			boolean isEmpty = !parameterBag.hasParameters();
			writeStartStrolchElement(Tags.PARAMETER_BAG, isEmpty, parameterBag);
			if (!isEmpty) {
				writeParameters(parameterBag);
				writer.writeEndElement();
			}
		}
	}
}
