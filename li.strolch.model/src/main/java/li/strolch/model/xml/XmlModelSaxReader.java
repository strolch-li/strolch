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

import java.text.MessageFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.ModelStatistics;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.DurationParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StringSetTimedState;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.impl.AString;
import li.strolch.model.timevalue.impl.BooleanValue;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.StringSetValue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlModelSaxReader extends DefaultHandler {

	protected static final Logger logger = LoggerFactory.getLogger(XmlModelSaxReader.class);

	protected StrolchElementListener listener;
	protected ModelStatistics statistics;

	private GroupedParameterizedElement parameterizedElement;
	private ParameterBag pBag;
	private StrolchTimedState<? extends IValue<?>> state;
	private String stateType;

	public XmlModelSaxReader(StrolchElementListener listener) {
		this.listener = listener;
		this.statistics = new ModelStatistics();
	}

	/**
	 * @return the statistics
	 */
	public ModelStatistics getStatistics() {
		return this.statistics;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		// TODO split each root object into its own file
		switch (qName) {

		case Tags.STROLCH_MODEL:
			break;

		case Tags.RESOURCE:

			String resId = attributes.getValue(Tags.ID);
			String resName = attributes.getValue(Tags.NAME);
			String resType = attributes.getValue(Tags.TYPE);
			Resource resource = new Resource(resId, resName, resType);
			this.parameterizedElement = resource;
			break;

		case Tags.ORDER:
			String orderId = attributes.getValue(Tags.ID);
			String orderName = attributes.getValue(Tags.NAME);
			String orderType = attributes.getValue(Tags.TYPE);
			String orderDateS = attributes.getValue(Tags.DATE);
			String orderStateS = attributes.getValue(Tags.STATE);
			Order order = new Order(orderId, orderName, orderType);
			if (orderDateS != null) {
				Date orderDate = ISO8601FormatFactory.getInstance().getDateFormat().parse(orderDateS);
				order.setDate(orderDate);
			}
			if (orderStateS != null) {
				State orderState = State.valueOf(orderStateS);
				order.setState(orderState);
			}
			this.parameterizedElement = order;
			break;

		case Tags.PARAMETER_BAG:
			String pBagId = attributes.getValue(Tags.ID);
			String pBagName = attributes.getValue(Tags.NAME);
			String pBagType = attributes.getValue(Tags.TYPE);
			ParameterBag pBag = new ParameterBag(pBagId, pBagName, pBagType);
			this.pBag = pBag;
			this.parameterizedElement.addParameterBag(pBag);
			break;

		case Tags.PARAMETER:

			// TODO refactor this code into using visitors

			String paramId = attributes.getValue(Tags.ID);
			String paramName = attributes.getValue(Tags.NAME);
			String paramType = attributes.getValue(Tags.TYPE);
			String paramValue = attributes.getValue(Tags.VALUE);
			String paramHiddenS = attributes.getValue(Tags.HIDDEN);
			String paramIndexS = attributes.getValue(Tags.INDEX);
			try {
				int index = StringHelper.isEmpty(paramIndexS) ? 0 : Integer.valueOf(paramIndexS);
				boolean paramHidden = StringHelper.isEmpty(paramHiddenS) ? false : StringHelper
						.parseBoolean(paramHiddenS);
				String paramUom = attributes.getValue(Tags.UOM);
				String paramInterpretation = attributes.getValue(Tags.INTERPRETATION);
				Parameter<?> param;
				switch (paramType) {
				case StringParameter.TYPE:
					param = new StringParameter(paramId, paramName, paramValue);
					break;
				case IntegerParameter.TYPE:
					param = new IntegerParameter(paramId, paramName, IntegerParameter.parseFromString(paramValue));
					break;
				case BooleanParameter.TYPE:
					param = new BooleanParameter(paramId, paramName, BooleanParameter.parseFromString(paramValue));
					break;
				case LongParameter.TYPE:
					param = new LongParameter(paramId, paramName, LongParameter.parseFromString(paramValue));
					break;
				case DateParameter.TYPE:
					param = new DateParameter(paramId, paramName, DateParameter.parseFromString(paramValue));
					break;
				case DurationParameter.TYPE:
					param = new DurationParameter(paramId, paramName, DurationParameter.parseFromString(paramValue));
					break;
				case StringListParameter.TYPE:
					param = new StringListParameter(paramId, paramName, StringListParameter.parseFromString(paramValue));
					break;
				case FloatParameter.TYPE:
					param = new FloatParameter(paramId, paramName, FloatParameter.parseFromString(paramValue));
					break;
				default:
					throw new UnsupportedOperationException(MessageFormat.format(
							"Parameters of type {0} are not supported!", paramType)); //$NON-NLS-1$
				}
				param.setHidden(paramHidden);
				param.setUom(paramUom);
				param.setInterpretation(paramInterpretation);
				param.setIndex(index);
				this.pBag.addParameter(param);
			} catch (Exception e) {
				throw new StrolchException("Failed to instantiate parameter " + paramId + " for bag "
						+ this.pBag.getLocator() + " due to " + e.getMessage(), e);
			}
			break;

		case Tags.TIMED_STATE:
			String stateId = attributes.getValue(Tags.ID);
			String stateName = attributes.getValue(Tags.NAME);
			this.stateType = attributes.getValue(Tags.TYPE);

			switch (this.stateType) {
			case FloatTimedState.TYPE:
				this.state = new FloatTimedState(stateId, stateName);
				break;
			case IntegerTimedState.TYPE:
				this.state = new IntegerTimedState(stateId, stateName);
				break;
			case BooleanTimedState.TYPE:
				this.state = new BooleanTimedState(stateId, stateName);
				break;
			case StringSetTimedState.TYPE:
				this.state = new StringSetTimedState(stateId, stateName);
				break;
			default:
				break;
			}

			break;

		case Tags.VALUE:
			String valueTime = attributes.getValue(Tags.TIME);
			Date date = ISO8601FormatFactory.getInstance().parseDate(valueTime);
			long time = date.getTime();
			String valueValue = attributes.getValue(Tags.VALUE);
			switch (this.stateType) {
			case FloatTimedState.TYPE:
				((FloatTimedState) this.state).getTimeEvolution().setValueAt(time, new FloatValue(valueValue));
				break;
			case IntegerTimedState.TYPE:
				((IntegerTimedState) this.state).getTimeEvolution().setValueAt(time, new IntegerValue(valueValue));
				break;
			case BooleanTimedState.TYPE:
				((BooleanTimedState) this.state).getTimeEvolution().setValueAt(time, new BooleanValue(valueValue));
				break;
			case StringSetTimedState.TYPE:

				Set<AString> value = new HashSet<>();
				String[] values = valueValue.split(",");
				for (String s : values) {
					value.add(new AString(s.trim()));
				}

				StringSetValue stringSetValue = new StringSetValue(value);
				((StringSetTimedState) this.state).getTimeEvolution().setValueAt(time, stringSetValue);
				break;
			default:
				break;
			}
			break;

		default:
			throw new IllegalArgumentException(MessageFormat.format("The element ''{0}'' is unhandled!", qName)); //$NON-NLS-1$
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {

		switch (qName) {
		case Tags.STROLCH_MODEL:
			break;
		case Tags.RESOURCE:
			this.listener.notifyResource((Resource) this.parameterizedElement);
			this.statistics.nrOfResources++;
			this.parameterizedElement = null;
			break;
		case Tags.ORDER:
			this.listener.notifyOrder((Order) this.parameterizedElement);
			this.statistics.nrOfOrders++;
			this.parameterizedElement = null;
			break;
		case Tags.PARAMETER_BAG:
			this.pBag = null;
			break;
		case Tags.PARAMETER:
			break;
		case Tags.INCLUDE_FILE:
			break;
		case Tags.TIMED_STATE:
			((Resource) this.parameterizedElement).addTimedState(this.state);
			break;
		case Tags.VALUE:
			break;
		default:
			throw new IllegalArgumentException(MessageFormat.format("The element ''{0}'' is unhandled!", qName)); //$NON-NLS-1$
		}
	}
}
