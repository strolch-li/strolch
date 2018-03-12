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

import li.strolch.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class NameSelection extends StrolchElementSelection {

	private StringMatchMode matchMode;
	private String name;

	/**
	 * @param name
	 * @param matchMode
	 */
	public NameSelection(String name, StringMatchMode matchMode) {
		this.name = name;
		this.matchMode = matchMode;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @return the matchMode
	 */
	public StringMatchMode getMatchMode() {
		return this.matchMode;
	}

	@Override
	public void accept(StrolchElementSelectionVisitor visitor) {
		visitor.visit(this);
	}
}
