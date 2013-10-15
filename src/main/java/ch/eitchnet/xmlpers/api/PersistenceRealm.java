package ch.eitchnet.xmlpers.api;

import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;

public interface PersistenceRealm {

	/**
	 * @return the realm
	 */
	public abstract String getRealmName();

	public abstract PersistenceContextFactoryDelegator getCtxFactoryDelegator();

	/**
	 * @return the objectRefCache
	 */
	public abstract ObjectReferenceCache getObjectRefCache();

	/**
	 * @return the persistenceManager
	 */
	public abstract PersistenceManager getPersistenceManager();

}