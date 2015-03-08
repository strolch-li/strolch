package ch.eitchnet.utils.xml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.annotation.XmlAttribute;

public class XmlKeyValue {

	@XmlAttribute(name = "key")
	private String key;
	@XmlAttribute(name = "value")
	private String value;

	public XmlKeyValue(String key, String value) {
		this.key = key;
		this.value = value;
	}

	public XmlKeyValue() {
		// no-arg constructor for JAXB
	}

	/**
	 * @return the key
	 */
	public String getKey() {
		return this.key;
	}

	/**
	 * @param key
	 *            the key to set
	 */
	public void setKey(String key) {
		this.key = key;
	}

	/**
	 * @return the value
	 */
	public String getValue() {
		return this.value;
	}

	/**
	 * @param value
	 *            the value to set
	 */
	public void setValue(String value) {
		this.value = value;
	}

	public static List<XmlKeyValue> valueOf(Map<String, String> map) {
		List<XmlKeyValue> keyValues = new ArrayList<>(map.size());
		for (Entry<String, String> entry : map.entrySet()) {
			keyValues.add(new XmlKeyValue(entry.getKey(), entry.getValue()));
		}
		return keyValues;
	}

	public static Map<String, String> toMap(List<XmlKeyValue> values) {
		Map<String, String> propertyMap = new HashMap<>(values.size());
		for (XmlKeyValue xmlKeyValue : values) {
			propertyMap.put(xmlKeyValue.getKey(), xmlKeyValue.getValue());
		}

		return propertyMap;
	}
}