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

import java.io.File;
import java.text.MessageFormat;

import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceContextFactory;
import li.strolch.xmlpers.api.PersistenceContextFactoryDelegator;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.impl.PathBuilder;

public class IdOfSubTypeRef extends ObjectRef {

	private final String type;
	private final String subType;
	private final String id;

	public IdOfSubTypeRef(String realmName, String type, String subType, String id) {
		super(realmName, RefNameCreator.createIdOfSubTypeName(realmName, type, subType, id));
		this.type = type;
		this.subType = subType;
		this.id = id;
	}

	@Override
	public String getType() {
		return this.type;
	}

	public String getSubType() {
		return this.subType;
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
		return tx.getRealm().getObjectRefCache().getSubTypeRef(this.type, this.subType);
	}

	@Override
	public ObjectRef getChildIdRef(PersistenceTransaction tx, String id) {
		String msg = MessageFormat.format("Already a leaf: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public ObjectRef getChildTypeRef(PersistenceTransaction tx, String type) {
		String msg = MessageFormat.format("Already a leaf: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public File getPath(PathBuilder pathBuilder) {
		return pathBuilder.getIdOfSubTypePath(this.type, this.subType, this.id);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		PersistenceContextFactoryDelegator ctxFactoryDelegator = tx.getRealm().getCtxFactoryDelegator();
		PersistenceContextFactory<T> persistenceContextFactory = ctxFactoryDelegator.<T> getCtxFactory(this.type);
		return persistenceContextFactory.createCtx(this);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
		result = prime * result + ((this.realmName == null) ? 0 : this.realmName.hashCode());
		result = prime * result + ((this.subType == null) ? 0 : this.subType.hashCode());
		result = prime * result + ((this.type == null) ? 0 : this.type.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		IdOfSubTypeRef other = (IdOfSubTypeRef) obj;
		if (this.realmName == null) {
			if (other.realmName != null)
				return false;
		} else if (!this.realmName.equals(other.realmName))
			return false;
		if (this.id == null) {
			if (other.id != null)
				return false;
		} else if (!this.id.equals(other.id))
			return false;
		if (this.subType == null) {
			if (other.subType != null)
				return false;
		} else if (!this.subType.equals(other.subType))
			return false;
		if (this.type == null) {
			if (other.type != null)
				return false;
		} else if (!this.type.equals(other.type))
			return false;
		return true;
	}
}
