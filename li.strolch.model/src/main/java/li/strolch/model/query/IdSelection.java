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

import ch.eitchnet.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class IdSelection extends StrolchElementSelection {

	private List<String> ids;
	private StringMatchMode matchMode;

	/**
	 * Instantiate using {@link StringMatchMode#es()}
	 * 
	 * @param id
	 */
	public IdSelection(String id) {
		this.ids = new ArrayList<>(1);
		this.ids.add(id);
		this.matchMode = StringMatchMode.es();
	}

	/**
	 * @param id
	 * @param matchMode
	 */
	public IdSelection(String id, StringMatchMode matchMode) {
		this.ids = new ArrayList<>(1);
		this.ids.add(id);
		this.matchMode = matchMode;
	}

	/**
	 * Instantiate using {@link StringMatchMode#es()}
	 * 
	 * @param matchMode
	 * @param ids
	 */
	public IdSelection(String... ids) {
		this.matchMode = StringMatchMode.es();
		this.ids = Arrays.asList(ids);
	}

	/**
	 * @param matchMode
	 * @param ids
	 */
	public IdSelection(StringMatchMode matchMode, String... ids) {
		this.matchMode = matchMode;
		this.ids = Arrays.asList(ids);
	}

	/**
	 * @param ids
	 * @param matchMode
	 */
	public IdSelection(List<String> ids, StringMatchMode matchMode) {
		this.matchMode = matchMode;
		this.ids = ids;
	}

	/**
	 * Instantiate using {@link StringMatchMode#es()}
	 * 
	 * @param ids
	 * @param matchMode
	 */
	public IdSelection(List<String> ids) {
		this.matchMode = StringMatchMode.es();
		this.ids = ids;
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
	 * 
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
