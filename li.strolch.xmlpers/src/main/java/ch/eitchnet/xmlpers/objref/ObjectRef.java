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
package ch.eitchnet.xmlpers.objref;

import java.io.File;

import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.impl.PathBuilder;

public abstract class ObjectRef extends LockableObject {

	protected final String realmName;
	protected final String name;

	protected ObjectRef(String realmName, String name) {
		this.realmName = realmName;
		this.name = name;
	}

	public String getRealmName() {
		return this.realmName;
	}

	public String getName() {
		return this.name;
	}

	public abstract boolean isRoot();

	public abstract boolean isLeaf();

	public abstract String getType();

	public abstract ObjectRef getParent(PersistenceTransaction tx);

	public abstract ObjectRef getChildIdRef(PersistenceTransaction tx, String id);

	public abstract ObjectRef getChildTypeRef(PersistenceTransaction tx, String type);

	public abstract File getPath(PathBuilder pathBuilder);

	public abstract <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx);

	@Override
	public String toString() {
		return getName();
	}

	@Override
	public abstract boolean equals(Object obj);

	@Override
	public abstract int hashCode();
}
