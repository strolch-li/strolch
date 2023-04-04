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
import java.util.Objects;

import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceTransaction;

public class SubTypeRef extends ObjectRef {

	private final TypeRef parent;
	private final String subType;

	public SubTypeRef(TypeRef parent, String subType) {
		super(RefNameCreator.createSubTypeName(parent.getType(), subType));
		this.parent = parent;
		this.subType = subType;
	}

	@Override
	public String getType() {
		return this.parent.getType();
	}

	public String getSubType() {
		return this.subType;
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
		return tx.getManager().getObjectRefCache().getIdOfSubTypeRef(getType(), this.subType, id);
	}

	@Override
	public ObjectRef getChildTypeRef(PersistenceTransaction tx, String type) {
		String msg = MessageFormat.format("A SubType has no child type: {0}", getName());
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		String msg = MessageFormat
				.format("{0} is not a leaf and can thus not have a Persistence Context", getName());
		throw new UnsupportedOperationException(msg);
	}
}
