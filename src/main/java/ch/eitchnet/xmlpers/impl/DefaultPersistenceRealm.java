package ch.eitchnet.xmlpers.impl;

import ch.eitchnet.xmlpers.api.PersistenceContextFactoryDelegator;
import ch.eitchnet.xmlpers.api.PersistenceManager;
import ch.eitchnet.xmlpers.api.PersistenceRealm;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;

public class DefaultPersistenceRealm implements PersistenceRealm {
	private final PersistenceManager persistenceManager;
	private final PersistenceContextFactoryDelegator ctxFactory;
	private final String realmName;
	private final ObjectReferenceCache objectRefCache;
	private final PathBuilder pathBuilder;

	public DefaultPersistenceRealm(String realm, PersistenceManager persistenceManager,
			PersistenceContextFactoryDelegator ctxFactory, PathBuilder pathBuilder, ObjectReferenceCache objectRefCache) {
		this.ctxFactory = ctxFactory;
		this.pathBuilder = pathBuilder;
		this.realmName = realm;
		this.persistenceManager = persistenceManager;
		this.objectRefCache = objectRefCache;
	}

	/**
	 * @return the realm
	 */
	@Override
	public String getRealmName() {
		return this.realmName;
	}

	@Override
	public PersistenceContextFactoryDelegator getCtxFactoryDelegator() {
		return this.ctxFactory;
	}

	/**
	 * @return the objectRefCache
	 */
	@Override
	public ObjectReferenceCache getObjectRefCache() {
		return this.objectRefCache;
	}

	/**
	 * @return the persistenceManager
	 */
	@Override
	public PersistenceManager getPersistenceManager() {
		return this.persistenceManager;
	}

	/**
	 * @return the pathBuilder
	 */
	PathBuilder getPathBuilder() {
		return this.pathBuilder;
	}
}