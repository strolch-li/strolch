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
package li.strolch.utils.collections;

import java.util.Objects;

/**
 * Simple wrapper for two elements
 *
 * @param <T> first object
 * @param <U> second object
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class TypedTuple<T, U> {

	private T first;
	private U second;

	public TypedTuple() {
		//
	}

	public TypedTuple(T first, U second) {
		this.first = first;
		this.second = second;
	}

	public T getFirst() {
		return this.first;
	}

	public void setFirst(T first) {
		this.first = first;
	}

	public U getSecond() {
		return this.second;
	}

	public void setSecond(U second) {
		this.second = second;
	}

	public boolean hasFirst() {
		return this.first != null;
	}

	public boolean hasSecond() {
		return this.second != null;
	}

	public boolean hasBoth() {
		return this.hasFirst() & this.hasSecond();
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		TypedTuple<?, ?> that = (TypedTuple<?, ?>) o;
		return Objects.equals(first, that.first) && Objects.equals(second, that.second);
	}

	@Override
	public int hashCode() {
		return Objects.hash(first, second);
	}
}
