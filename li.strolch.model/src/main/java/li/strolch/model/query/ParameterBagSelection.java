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
public class ParameterBagSelection implements Selection {

	private String bagKey;

	public ParameterBagSelection(String bagKey) {
		this.bagKey = bagKey;
	}

	/**
	 * @return the bagKey
	 */
	public String getBagKey() {
		return this.bagKey;
	}

	@Override
	public boolean hasSelection() {
		return true;
	}

	@Override
	public void accept(QueryVisitor visitor) {
		accept((StrolchRootElementSelectionVisitor) visitor);
	}

	public void accept(StrolchRootElementSelectionVisitor visitor) {
		visitor.visit(this);
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(getClass().getSimpleName() + " [bagKey=");
		builder.append(this.bagKey);
		builder.append("]");
		return builder.toString();
	}

	public static class NullParameterBagSelection extends ParameterBagSelection {

		public NullParameterBagSelection(String bagKey) {
			super(bagKey);
		}

		@Override
		public void accept(StrolchRootElementSelectionVisitor visitor) {
			visitor.visit(this);
		}
	}
}
