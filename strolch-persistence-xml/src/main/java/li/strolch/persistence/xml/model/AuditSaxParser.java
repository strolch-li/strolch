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

import javax.xml.stream.XMLStreamWriter;

import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditSaxReader;
import li.strolch.model.audit.AuditToSaxWriterVisitor;
import li.strolch.xmlpers.api.SaxParser;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditSaxParser implements SaxParser<Audit> {

	private Audit audit;

	@Override
	public Audit getObject() {
		return this.audit;
	}

	@Override
	public void setObject(Audit audit) {
		this.audit = audit;
	}

	@Override
	public DefaultHandler getDefaultHandler() {
		return new AuditSaxReader(audit1 -> this.audit = audit1);
	}

	@Override
	public void write(XMLStreamWriter xmlWriter) {
		this.audit.accept(new AuditToSaxWriterVisitor(xmlWriter));
	}
}
