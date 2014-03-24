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

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class NotSelection extends BooleanSelection {

	/**
	 * @param selection
	 */
	public NotSelection(Selection selection) {
		super(selection);
	}

	/**
	 * @throws UnsupportedOperationException because a {@link NotSelection} can only work on a single {@link Selection}
	 */
	@Override
	public NotSelection with(Selection selection) {
		throw new UnsupportedOperationException("NotSelection can only have a single selection"); //$NON-NLS-1$
	}

	public Selection getSelection() {
		return this.selections.get(0);
	}

	@Override
	public void accept(QueryVisitor visitor) {
		visitor.visitNot(this);
	}
}
