package li.strolch.persistence.impl.model;

import li.strolch.model.Resource;
import li.strolch.model.Tags;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceContextFactory;
import ch.eitchnet.xmlpers.objref.IdOfSubTypeRef;
import ch.eitchnet.xmlpers.objref.ObjectRef;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;

public class ResourceContextFactory implements PersistenceContextFactory<Resource> {

	@Override
	public PersistenceContext<Resource> createCtx(ObjectRef objectRef) {
		PersistenceContext<Resource> ctx = new PersistenceContext<>(objectRef);
		ctx.setParserFactory(new ResourceParserFactory());
		return ctx;
	}

	@Override
	public PersistenceContext<Resource> createCtx(ObjectReferenceCache objectRefCache, Resource t) {
		IdOfSubTypeRef objectRef = objectRefCache.getIdOfSubTypeRef(Tags.RESOURCE, t.getType(), t.getId());
		return createCtx(objectRef);
	}
}