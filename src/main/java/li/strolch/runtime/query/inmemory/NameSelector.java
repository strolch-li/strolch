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
import ch.eitchnet.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class NameSelector<T extends StrolchElement> implements Selector<T> {

	private StringMatchMode matchMode;
	private String name;

	public NameSelector(String name, StringMatchMode matchMode) {
		this.name = name;
		this.matchMode = matchMode;
	}

	@Override
	public boolean select(T element) {
		String name = element.getName();
		return this.matchMode.matches(this.name, name);
	}

	/**
	 * @return the matchMode
	 */
	public StringMatchMode getMatchMode() {
		return this.matchMode;
	}
}
