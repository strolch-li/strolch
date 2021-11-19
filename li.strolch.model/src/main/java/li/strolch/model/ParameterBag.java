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
package li.strolch.model;

import li.strolch.model.Locator.LocatorBuilder;
import li.strolch.model.visitor.StrolchElementVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ParameterBag extends ParameterizedElement {

	/**
	 * Empty Constructor
	 */
	public ParameterBag() {
		// 
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param type
	 * 		the type
	 */
	public ParameterBag(String id, String name, String type) {
		super(id, name, type);
	}

	@Override
	public ParameterBag getClone() {
		ParameterBag clone = new ParameterBag();
		super.fillClone(clone);
		return clone;
	}

	@Override
	public void fillLocator(LocatorBuilder lb) {
		if (this.parent != null)
			this.parent.fillLocator(lb);
		lb.append(Tags.BAG);
		lb.append(this.id);
	}

	@Override
	public boolean isRootElement() {
		return false;
	}

	public void removeAllParameters() {
		assertNotReadonly();
		if (this.parameterMap != null)
			this.parameterMap.clear();
	}

	@Override
	public <T> T accept(StrolchElementVisitor<T> visitor) {
		return visitor.visitParameterBag(this);
	}
}
