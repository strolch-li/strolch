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
import java.util.Objects;

import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.impl.PathBuilder;

public class RootRef extends ObjectRef {

	public RootRef() {
		super(RefNameCreator.createRootName());
	}

	@Override
	public boolean isRoot() {
		return true;
	}

	@Override
	public boolean isLeaf() {
		return false;
	}

	@Override
	public String getType() {
		String msg = MessageFormat.format("RootRef has no type: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public ObjectRef getParent(PersistenceTransaction tx) {
		String msg = MessageFormat.format("RootRef has no parent: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public ObjectRef getChildIdRef(PersistenceTransaction tx, String id) {
		String msg = MessageFormat.format("RootRef has no child id: {0}", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
	}

	@Override
	public ObjectRef getChildTypeRef(PersistenceTransaction tx, String type) {
		return tx.getManager().getObjectRefCache().getTypeRef(type);
	}

	@Override
	public File getPath(PathBuilder pathBuilder) {
		return pathBuilder.getRootPath();
	}

	@Override
	public <T> PersistenceContext<T> createPersistenceContext(PersistenceTransaction tx) {
		String msg = MessageFormat
				.format("{0} is not a leaf and can thus not have a Persistence Context", getName()); //$NON-NLS-1$
		throw new UnsupportedOperationException(msg);
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
}
