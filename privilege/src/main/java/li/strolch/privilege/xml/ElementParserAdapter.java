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
package li.strolch.privilege.xml;

import org.xml.sax.Attributes;

public abstract class ElementParserAdapter implements ElementParser {

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) {
		// empty implementation
	}

	@Override
	public void characters(char[] ch, int start, int length) {
		// empty implementation
	}

	@Override
	public void endElement(String uri, String localName, String qName) {
		// empty implementation
	}

	@Override
	public void notifyChild(ElementParser child) {
		// empty implementation
	}
}