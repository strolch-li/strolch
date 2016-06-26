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

import java.util.List;

import li.strolch.model.StrolchElement;
import li.strolch.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class IdSelector<T extends StrolchElement> implements Selector<T> {

	private StringMatchMode matchMode;
	private List<String> ids;

	/**
	 * @param ids
	 * @param matchMode
	 */
	public IdSelector(List<String> ids, StringMatchMode matchMode) {
		this.ids = ids;
		this.matchMode = matchMode;
	}

	/**
	 * @return the ids
	 */
	public List<String> getIds() {
		return this.ids;
	}

	/**
	 * @return the matchMode
	 */
	public StringMatchMode getMatchMode() {
		return this.matchMode;
	}

	/**
	 * @param id
	 * @return
	 */
	public IdSelector<T> with(String id) {
		this.ids.add(id);
		return this;
	}

	@Override
	public boolean select(StrolchElement element) {
		if (this.ids.isEmpty())
			return true;

		String elemId = element.getId();
		if (this.ids.size() == 1)
			return this.matchMode.matches(elemId, this.ids.get(0));

		for (String id : this.ids) {
			if (this.matchMode.matches(elemId, id))
				return true;
		}

		return false;
	}
}
