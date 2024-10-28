package li.strolch.privilege.xml;

import org.xml.sax.Attributes;

import java.util.HashMap;
import java.util.Map;

import static li.strolch.privilege.helper.XmlConstants.*;

class PropertyParser extends ElementParserAdapter {

	// <Property name="organizationalUnit" value="Development" />

	public final Map<String, String> parameterMap = new HashMap<>();

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) {

		if (qName.equals(PROPERTY)) {
			String key = attributes.getValue(ATTR_NAME).trim();
			String value = attributes.getValue(ATTR_VALUE).trim();
			this.parameterMap.put(key, value);
		} else {
			if (!qName.equals(PROPERTIES)) {
				throw new IllegalArgumentException("Unhandled tag " + qName);
			}
		}
	}

	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}
}
