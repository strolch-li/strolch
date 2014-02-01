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
import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class StrolchQuery<T extends QueryVisitor> {

	private Navigation navigation;
	protected List<Selection> selections;

	public StrolchQuery(Navigation navigation) {
		this.navigation = navigation;
		this.selections = new ArrayList<>();
	}

	public void addSelection(BooleanSelection selection) {
		this.selections.add(selection);
	}

	public void addSelection(StrolchElementSelection selection) {
		this.selections.add(selection);
	}

	public void addSelection(ParameterSelection selection) {
		this.selections.add(selection);
	}

	public StrolchQuery<T> select(Selection selection) {
		this.selections.add(selection);
		return this;
	}

	public StrolchQuery<T> and(Selection... selections) {
		AndSelection and = new AndSelection(selections);
		this.selections.add(and);
		return this;
	}

	public StrolchQuery<T> or(Selection... selections) {
		OrSelection or = new OrSelection(selections);
		this.selections.add(or);
		return this;
	}

	public StrolchQuery<T> not(Selection selection) {
		NotSelection not = new NotSelection(selection);
		this.selections.add(not);
		return this;
	}

	public void accept(T visitor) {
		this.navigation.accept(visitor);
		for (Selection selection : this.selections) {
			selection.accept(visitor);
		}
	}
}
