/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
