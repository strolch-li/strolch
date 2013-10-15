package ch.eitchnet.xmlpers.test.impl;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceContextFactory;
import ch.eitchnet.xmlpers.objref.IdOfSubTypeRef;
import ch.eitchnet.xmlpers.objref.ObjectRef;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;
import ch.eitchnet.xmlpers.test.model.Resource;

public class ResourceContextFactory implements PersistenceContextFactory<Resource> {

	@Override
	public PersistenceContext<Resource> createCtx(ObjectReferenceCache objectRefCache, Resource t) {
		IdOfSubTypeRef objectRef = objectRefCache.getIdOfSubTypeRef(TestConstants.TYPE_RES, t.getType(), t.getId());
		return createCtx(objectRef);
	}

	@Override
	public PersistenceContext<Resource> createCtx(ObjectRef objectRef) {
		PersistenceContext<Resource> ctx = new PersistenceContext<>(objectRef);
		ctx.setParserFactory(new ResourceParserFactory());
		return ctx;
	}

}