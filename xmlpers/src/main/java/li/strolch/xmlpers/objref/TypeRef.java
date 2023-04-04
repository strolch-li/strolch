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
import li.strolch.xmlpers.api.PersistenceTransaction;

public class TypeRef extends ObjectRef {

	private final ObjectRef parent;
	private final String type;

	public TypeRef(ObjectRef parent, String type) {
		super(RefNameCreator.createTypeName(type));
		this.parent = parent;
		this.type = type;
	}

	@Override
	public String getType() {
		return this.type;
	}

	@Override
	public boolean isRoot() {
		return false;
	}

	@Override
	public boolean isLeaf() {
		return false;
	}

	@Override
	public ObjectRef getParent(PersistenceTransaction tx) {
		return this.parent;
	}

	@Override
	public ObjectRef getChildIdRef(PersistenceTransaction tx, String id) {
		return tx.getManager().getObjectRefCache().getIdOfTypeRef(this.type, id);
	}

	@Override
	public ObjectRef getChildTypeRef(PersistenceTransaction tx, String type) {
		return tx.getManager().getObjectRefCache().getSubTypeRef(this.type, type);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		String msg = MessageFormat
				.format("{0} is not a leaf and can thus not have a Persistence Context", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
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
		TypeRef other = (TypeRef) obj;
		if (this.name == null) {
			if (other.name != null)
				return false;
		} else if (!this.name.equals(other.name))
			return false;
		if (this.type == null) {
			return other.type == null;
		} else
			return this.type.equals(other.type);
	}
}
