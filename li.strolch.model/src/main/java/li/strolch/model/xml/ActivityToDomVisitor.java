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

import javax.xml.parsers.DocumentBuilder;

import li.strolch.model.ActivityVisitor;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.utils.helper.DomUtil;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ActivityToDomVisitor extends StrolchElementToDomVisitor implements ActivityVisitor<Document> {

	@Override
	public Document visit(Activity activity) {
		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		this.document = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element asDom = toDom(activity);
		document.appendChild(asDom);
		return this.document;
	}

	public Element toDom(Action action) {
		return super.toDom(action);
	}

	public Element toDom(Activity activity) {
		return super.toDom(activity);
	}
}
