package ch.eitchnet.xmlpers.test.impl;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceContextFactory;
import ch.eitchnet.xmlpers.objref.IdOfTypeRef;
import ch.eitchnet.xmlpers.objref.ObjectRef;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;
import ch.eitchnet.xmlpers.test.model.Book;

public class BookContextFactory implements PersistenceContextFactory<Book> {

	@Override
	public PersistenceContext<Book> createCtx(ObjectRef objectRef) {
		PersistenceContext<Book> ctx = new PersistenceContext<>(objectRef);
		ctx.setParserFactory(new BookParserFactory());
		return ctx;
	}

	@Override
	public PersistenceContext<Book> createCtx(ObjectReferenceCache objectRefCache, Book t) {
		IdOfTypeRef objectRef = objectRefCache.getIdOfTypeRef(TestConstants.TYPE_BOOK, t.getId().toString());
		return createCtx(objectRef);
	}
}
