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
package li.strolch.xmlpers.objref;

import java.text.MessageFormat;

import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceContextFactory;
import li.strolch.xmlpers.api.PersistenceContextFactoryDelegator;
import li.strolch.xmlpers.api.PersistenceTransaction;

public class IdOfTypeRef extends ObjectRef {

	private final TypeRef parent;
	private final String id;

	public IdOfTypeRef(TypeRef parent, String id) {
		super(RefNameCreator.createIdOfTypeName(parent.getType(), id));
		this.parent = parent;
		this.id = id;
	}

	@Override
	public String getType() {
		return this.parent.getType();
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return this.id;
	}

	@Override
	public boolean isRoot() {
		return false;
	}

	@Override
	public boolean isLeaf() {
		return true;
	}

	@Override
	public ObjectRef getParent(PersistenceTransaction tx) {
		return this.parent;
	}

	@Override
	public ObjectRef getChildIdRef(PersistenceTransaction tx, String id) {
		String msg = MessageFormat.format("Already a leaf: {0}", getName());
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public ObjectRef getChildTypeRef(PersistenceTransaction tx, String type) {
		String msg = MessageFormat.format("Already a leaf: {0}", getName());
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		PersistenceContextFactoryDelegator ctxFactoryDelegator = tx.getManager().getCtxFactory();
		PersistenceContextFactory<T> persistenceContextFactory = ctxFactoryDelegator.getCtxFactory(getType());
		return persistenceContextFactory.createCtx(this);
	}
}
