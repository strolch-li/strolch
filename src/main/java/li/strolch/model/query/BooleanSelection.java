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
import java.util.Collections;
import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class BooleanSelection implements Selection {

	protected List<Selection> selections;

	public BooleanSelection() {
		this.selections = new ArrayList<>(1);
	}

	public BooleanSelection(List<Selection> selections) {
		this.selections = selections;
	}

	public BooleanSelection(Selection leftHandSide, Selection rightHandSide) {
		this.selections = new ArrayList<>(2);
		this.selections.add(leftHandSide);
		this.selections.add(rightHandSide);
	}

	public BooleanSelection(Selection... selections) {
		this.selections = Arrays.asList(selections);
	}

	public List<Selection> getSelections() {
		return Collections.unmodifiableList(this.selections);
	}

	public BooleanSelection with(Selection selection) {
		this.selections.add(selection);
		return this;
	}

	public BooleanSelection with(Selection... selections) {
		for (Selection selection : selections) {
			this.selections.add(selection);
		}
		return this;
	}

	public BooleanSelection with(List<Selection> selections) {
		this.selections.addAll(selections);
		return this;
	}

	@Override
	public abstract void accept(QueryVisitor visitor);
}
