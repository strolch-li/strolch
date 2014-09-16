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
package li.strolch.model.query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class IdSelection extends StrolchElementSelection {

	private List<String> ids;

	public IdSelection() {
		this.ids = new ArrayList<>(1);
	}

	/**
	 * @param id
	 */
	public IdSelection(String id) {
		this.ids = new ArrayList<>(1);
		this.ids.add(id);
	}

	/**
	 * @param ids
	 */
	public IdSelection(String... ids) {
		this.ids = Arrays.asList(ids);
	}

	/**
	 * @param ids
	 */
	public IdSelection(List<String> ids) {
		this.ids = ids;
	}

	/**
	 * @return the ids
	 */
	public List<String> getIds() {
		return this.ids;
	}

	/**
	 * @param id
	 * @return
	 */
	public IdSelection with(String id) {
		this.ids.add(id);
		return this;
	}

	@Override
	public void accept(StrolchElementSelectionVisitor visitor) {
		visitor.visit(this);
	}
}
