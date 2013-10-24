package li.strolch.persistence.impl.model;

import javax.xml.parsers.DocumentBuilder;

import li.strolch.model.Order;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.xmlpers.api.DomParser;
import ch.eitchnet.xmlpers.util.DomUtil;

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

		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		Document document = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element orderDom = this.order.toDom(document);
		document.appendChild(orderDom);

		return document;
	}

	@Override
	public void fromDom(Document document) {

		Element rootElement = document.getDocumentElement();
		Order order = new Order(rootElement);
		this.order = order;
	}
}
