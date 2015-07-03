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
package li.strolch.model.xml;

import li.strolch.model.Order;
import li.strolch.model.State;
import li.strolch.model.Tags;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class OrderFromDomVisitor extends StrolchElementFromDomVisitor {

	public Order visit(Document doc) {
		return fromDom(doc.getDocumentElement());
	}

	public Order fromDom(Element element) {
		Order order = new Order();
		fromDom(element, order);

		String date = element.getAttribute(Tags.DATE);
		String state = element.getAttribute(Tags.STATE);

		if (StringHelper.isEmpty(date)) {
			order.setDate(ISO8601FormatFactory.getInstance().getDateFormat().parse("-")); //$NON-NLS-1$
		} else {
			order.setDate(ISO8601FormatFactory.getInstance().getDateFormat().parse(date));
		}

		if (state == null || state.isEmpty()) {
			order.setState(State.CREATED);
		} else {
			order.setState(State.valueOf(state));
		}

		return order;
	}
}
