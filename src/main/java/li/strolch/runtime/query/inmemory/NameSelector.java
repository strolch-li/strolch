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
package li.strolch.runtime.query.inmemory;

import li.strolch.model.StrolchElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class NameSelector<T extends StrolchElement> implements Selector<T> {

	private String name;
	private boolean contains;
	private boolean caseInsensitive;

	public NameSelector(String name) {
		this.name = name;
	}

	@Override
	public boolean select(T element) {
		String name = element.getName();
		if (this.contains && this.caseInsensitive)
			return name.toLowerCase().contains(this.name.toLowerCase());

		if (this.caseInsensitive)
			return name.toLowerCase().equals(this.name.toLowerCase());

		if (this.contains)
			return name.contains(this.name);

		return name.equals(this.name);
	}

	public boolean isContains() {
		return this.contains;
	}

	public boolean isCaseInsensitive() {
		return this.caseInsensitive;
	}

	public NameSelector<T> contains(boolean contains) {
		this.contains = contains;
		return this;
	}

	public NameSelector<T> caseInsensitive(boolean caseInsensitive) {
		this.caseInsensitive = caseInsensitive;
		return this;
	}
}
