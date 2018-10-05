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
package li.strolch.xmlpers.api;

import li.strolch.xmlpers.objref.ObjectRef;

public class PersistenceContext<T> {

	private final ObjectRef objectRef;
	private T object;
	private ParserFactory<T> parserFactory;

	public PersistenceContext(ObjectRef objectRef) {
		this.objectRef = objectRef;
	}

	public ObjectRef getObjectRef() {
		return this.objectRef;
	}

	public T getObject() {
		return this.object;
	}

	public void setObject(T object) {
		this.object = object;
	}

	public ParserFactory<T> getParserFactor() {
		return this.parserFactory;
	}

	public void setParserFactory(ParserFactory<T> parserFactory) {
		this.parserFactory = parserFactory;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.objectRef == null) ? 0 : this.objectRef.hashCode());
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
		PersistenceContext<?> other = (PersistenceContext<?>) obj;
		if (this.objectRef == null) {
			if (other.objectRef != null)
				return false;
		} else if (!this.objectRef.equals(other.objectRef))
			return false;
		return true;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("PersistenceContext [objectRef=");
		builder.append(this.objectRef);
		builder.append(", object=");
		builder.append(this.object);
		builder.append(", parserFactory=");
		builder.append(this.parserFactory);
		builder.append("]");
		return builder.toString();
	}
}