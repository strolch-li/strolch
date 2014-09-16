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

import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class OrSelection extends BooleanSelection {

	public OrSelection() {
		super();
	}

	public OrSelection(List<Selection> selections) {
		super(selections);
	}

	@SafeVarargs
	public OrSelection(Selection... selections) {
		super(selections);
	}

	@Override
	public OrSelection with(Selection selection) {
		super.with(selection);
		return this;
	}

	@Override
	public OrSelection with(List<Selection> selections) {
		super.with(selections);
		return this;
	}

	@Override
	public OrSelection with(Selection... selections) {
		super.with(selections);
		return this;
	}

	@Override
	public void accept(QueryVisitor visitor) {
		visitor.visitOr(this);
	}
}
