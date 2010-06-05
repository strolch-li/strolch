/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.helper;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.dom4j.Element;

import ch.eitchnet.privilege.base.XmlConstants;

/**
 * @author rvonburg
 * 
 */
public class ConfigurationHelper {

	@SuppressWarnings("unchecked")
	public static Map<String, String> convertToParameterMap(Element element) {

		Map<String, String> parameterMap = new HashMap<String, String>();

		List<Element> elements = element.elements(XmlConstants.XML_PARAMETER);
		for (Element parameter : elements) {
			String name = parameter.attributeValue(XmlConstants.XML_ATTR_NAME);
			String value = parameter.attributeValue(XmlConstants.XML_ATTR_VALUE);
			parameterMap.put(name, value);
		}

		return parameterMap;
	}
}
