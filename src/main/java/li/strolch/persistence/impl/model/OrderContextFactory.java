package li.strolch.persistence.impl.model;

import li.strolch.model.Order;
import li.strolch.model.Tags;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceContextFactory;
import ch.eitchnet.xmlpers.objref.IdOfSubTypeRef;
import ch.eitchnet.xmlpers.objref.ObjectRef;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;

public class OrderContextFactory implements PersistenceContextFactory<Order> {

	@Override
	public PersistenceContext<Order> createCtx(ObjectRef objectRef) {
		PersistenceContext<Order> ctx = new PersistenceContext<>(objectRef);
		ctx.setParserFactory(new OrderParserFactory());
		return ctx;
	}

	@Override
	public PersistenceContext<Order> createCtx(ObjectReferenceCache objectRefCache, Order t) {
		IdOfSubTypeRef objectRef = objectRefCache.getIdOfSubTypeRef(Tags.ORDER, t.getType(), t.getId());
		return createCtx(objectRef);
	}

}
