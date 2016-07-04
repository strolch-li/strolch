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
package li.strolch.xmlpers.impl;

import li.strolch.xmlpers.api.PersistenceContextFactoryDelegator;
import li.strolch.xmlpers.api.PersistenceManager;
import li.strolch.xmlpers.api.PersistenceRealm;
import li.strolch.xmlpers.objref.ObjectReferenceCache;

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