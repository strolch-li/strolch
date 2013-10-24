package li.strolch.persistence.impl.model;

import li.strolch.model.Order;
import ch.eitchnet.xmlpers.api.DomParser;
import ch.eitchnet.xmlpers.api.ParserFactory;
import ch.eitchnet.xmlpers.api.SaxParser;

public class OrderParserFactory implements ParserFactory<Order> {

	@Override
	public DomParser<Order> getDomParser() {
		return new OrderDomParser();
	}

	@Override
	public SaxParser<Order> getSaxParser() {
		throw new UnsupportedOperationException("Not yet implemented!"); //$NON-NLS-1$
	}
}
