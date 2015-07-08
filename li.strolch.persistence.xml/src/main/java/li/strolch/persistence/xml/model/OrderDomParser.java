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
package li.strolch.persistence.xml.model;

import li.strolch.model.Order;
import li.strolch.model.xml.OrderFromDomVisitor;
import li.strolch.model.xml.OrderToDomVisitor;

import org.w3c.dom.Document;

import ch.eitchnet.xmlpers.api.DomParser;

public class OrderDomParser implements DomParser<Order> {

	private Order order;

	@Override
	public Order getObject() {
		return this.order;
	}

	@Override
	public void setObject(Order object) {
		this.order = object;
	}

	@Override
	public Document toDom() {
		OrderToDomVisitor orderDomVisitor = new OrderToDomVisitor();
		orderDomVisitor.visit(this.order);
		return orderDomVisitor.getDocument();
	}

	@Override
	public void fromDom(Document document) {
		Order order = new OrderFromDomVisitor().visit(document);
		this.order = order;
	}
}
