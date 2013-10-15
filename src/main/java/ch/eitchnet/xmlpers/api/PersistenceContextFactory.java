package ch.eitchnet.xmlpers.api;

import ch.eitchnet.xmlpers.objref.ObjectRef;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;

public interface PersistenceContextFactory<T> {

	public PersistenceContext<T> createCtx(ObjectRef objectRef);

	public PersistenceContext<T> createCtx(ObjectReferenceCache objectRefCache, T t);

}