/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.agent;

import java.io.File;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Date;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ModelFileDefaultHandler extends DefaultHandler {

	private static final Logger logger = LoggerFactory.getLogger(ModelFileDefaultHandler.class);

	private File modelFile;

	private Resource resource;
	private Order order;
	private ParameterBag pBag;

	public ModelFileDefaultHandler(File modelFile) {
		this.modelFile = modelFile;
	}

	@SuppressWarnings("nls")
	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		switch (qName) {
		case Tags.RESOURCE:
			String resId = attributes.getValue(Tags.ID);
			String resName = attributes.getValue(Tags.NAME);
			String resType = attributes.getValue(Tags.TYPE);
			Resource resource = new Resource(resId, resName, resType);
			this.resource = resource;
			break;
		case Tags.ORDER:
			String orderId = attributes.getValue(Tags.ID);
			String orderName = attributes.getValue(Tags.NAME);
			String orderType = attributes.getValue(Tags.TYPE);
			String orderDateS = attributes.getValue(Tags.DATE);
			String orderStateS = attributes.getValue(Tags.STATE);
			Date orderDate = ISO8601FormatFactory.getInstance().getDateFormat().parse(orderDateS);
			State orderState = State.valueOf(orderStateS);
			Order order = new Order(orderId, orderName, orderType, orderDate, orderState);
			this.order = order;
			break;
		case Tags.PARAMETER_BAG:
			String pBagId = attributes.getValue(Tags.ID);
			String pBagName = attributes.getValue(Tags.NAME);
			String pBagType = attributes.getValue(Tags.TYPE);
			ParameterBag pBag = new ParameterBag(pBagId, pBagName, pBagType);
			this.pBag = pBag;
			break;
		case Tags.PARAMETER:
			String paramId = attributes.getValue(Tags.ID);
			String paramName = attributes.getValue(Tags.NAME);
			String paramType = attributes.getValue(Tags.TYPE);
			String paramValue = attributes.getValue(Tags.VALUE);
			String paramHiddenS = attributes.getValue(Tags.HIDDEN);
			boolean paramHidden = StringHelper.parseBoolean(paramHiddenS);
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
			case StringListParameter.TYPE:
				param = new StringListParameter(paramId, paramName, StringListParameter.parseFromString(paramValue));
				break;
			case FloatParameter.TYPE:
				param = new FloatParameter(paramId, paramName, FloatParameter.parseFromString(paramValue));
				break;
			default:
				throw new UnsupportedOperationException("Parameters of type " + paramType + " are not supported!");
			}
			param.setHidden(paramHidden);
			param.setUom(paramUom);
			param.setInterpretation(paramInterpretation);
			this.pBag.addParameter(param);
			break;
		default:
			throw new IllegalArgumentException("The element '" + qName + "' is unhandled!");
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {

		switch (qName) {
		case Tags.RESOURCE:
			this.resource = null;
			break;
		case Tags.ORDER:
			this.order = null;
			break;
		case Tags.PARAMETER_BAG:
			this.pBag = null;
			break;
		case Tags.PARAMETER:
			break;
		default:
			throw new IllegalArgumentException("The element '" + qName + "' is unhandled!");
		}
	}

	public void parseFile() {

		try {

			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();

			sp.parse(this.modelFile, this);

			String msg = "SAX parsed model file {0}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, this.modelFile.getAbsolutePath()));

		} catch (ParserConfigurationException | SAXException | IOException e) {

			String msg = "Parsing failed due to internal error: {0}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, e.getMessage()), e);
		}
	}
}
