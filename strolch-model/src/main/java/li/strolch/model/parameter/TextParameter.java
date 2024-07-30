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
package li.strolch.model.parameter;

import li.strolch.model.StrolchValueType;
import li.strolch.model.visitor.StrolchElementVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TextParameter extends StringParameter {

	/**
	 * Empty constructor
	 */
	public TextParameter() {
		//
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 * @param value
	 * 		the value
	 */
	public TextParameter(String id, String name, String value) {
		super(id, name, value);
	}

	@Override
	public String getType() {
		return StrolchValueType.TEXT.getType();
	}

	@Override
	public StrolchValueType getValueType() {
		return StrolchValueType.TEXT;
	}

	@Override
	public TextParameter getClone() {
		TextParameter clone = new TextParameter();
		super.fillClone(clone);
		clone.value = this.value;
		return clone;
	}

	@Override
	public <U> U accept(StrolchElementVisitor<U> visitor) {
		return visitor.visitTextParam(this);
	}
}
