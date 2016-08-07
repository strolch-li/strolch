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

import li.strolch.model.Resource;
import li.strolch.model.visitor.ResourceVisitor;

import org.xml.sax.ContentHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ResourceToSaxVisitor extends StrolchElementToSaxVisitor implements ResourceVisitor<Void> {

	public ResourceToSaxVisitor(ContentHandler contentHandler) {
		super(contentHandler);
	}

	@Override
	public Void visit(Resource res) {
		try {

			toSax(res);

		} catch (Exception e) {
			String msg = "Failed to transform Resource {0} to XML due to {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, res.getLocator(), e.getMessage());
			throw new RuntimeException(msg, e);
		}

		return null;
	}
}
