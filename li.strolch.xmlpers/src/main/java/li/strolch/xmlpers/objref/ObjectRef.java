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
import java.util.Objects;

import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.impl.PathBuilder;

public abstract class ObjectRef extends LockableObject implements Comparable<ObjectRef> {

	protected final String name;

	protected ObjectRef(String name) {
		this.name = name;
	}

	public String getName() {
		return this.name;
	}

	public File getPath(PathBuilder pathBuilder) {
		return pathBuilder.getPath(this);
	}

	public abstract boolean isRoot();

	public abstract boolean isLeaf();

	public abstract String getType();

	public abstract ObjectRef getParent(PersistenceTransaction tx);

	public abstract ObjectRef getChildIdRef(PersistenceTransaction tx, String id);

	public abstract ObjectRef getChildTypeRef(PersistenceTransaction tx, String type);

	public abstract <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx);

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		ObjectRef objectRef = (ObjectRef) o;
		return Objects.equals(this.name, objectRef.name);
	}

	@Override
	public int hashCode() {
		return Objects.hash(this.name);
	}

	@Override
	public int compareTo(ObjectRef o) {
		return this.name.compareTo(o.name);
	}
}
