/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.persistence.xml.model;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.xml.ActivityToSaxWriterVisitor;
import li.strolch.model.xml.StrolchElementListener;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.xmlpers.api.SaxParser;

import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ActivitySaxParser implements SaxParser<Activity> {

	protected Activity activity;

	@Override
	public Activity getObject() {
		return this.activity;
	}

	@Override
	public void setObject(Activity activity) {
		this.activity = activity;
	}

	@Override
	public DefaultHandler getDefaultHandler() {
		return new XmlModelSaxReader(new StrolchElementListener() {

			@Override
			public void notifyResource(Resource resource) {
				throw new IllegalArgumentException("Only expect Activities!");
			}

			@Override
			public void notifyOrder(Order order) {
				throw new IllegalArgumentException("Only expect Activities!");
			}

			@Override
			public void notifyActivity(Activity activity) {
				ActivitySaxParser.this.activity = activity;
			}
		});
	}

	@Override
	public void write(XMLStreamWriter xmlWriter) throws XMLStreamException {
		new ActivityToSaxWriterVisitor(xmlWriter);
	}
}
