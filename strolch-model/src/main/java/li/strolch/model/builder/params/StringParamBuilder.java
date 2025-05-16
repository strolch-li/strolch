/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.StringParameter;

import static li.strolch.model.StrolchModelConstants.*;

public class StringParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<String, StringParameter, T> {

	public StringParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	public StringParamBuilder<T> resourceRef(String type) {
		this.interpretation = INTERPRETATION_RESOURCE_REF;
		this.uom = type;
		return this;
	}

	public StringParamBuilder<T> orderRef(String type) {
		this.interpretation = INTERPRETATION_ORDER_REF;
		this.uom = type;
		return this;
	}

	public StringParamBuilder<T> activityRef(String type) {
		this.interpretation = INTERPRETATION_ACTIVITY_REF;
		this.uom = type;
		return this;
	}

	public StringParamBuilder<T> enumeration(Enum<?> defaultValue) {
		this.interpretation = INTERPRETATION_ENUMERATION;
		this.uom = defaultValue.getDeclaringClass().getSimpleName();
		value(defaultValue.name());
		return this;
	}

	@Override
	public StringParameter build() {
		return applyParameter(new StringParameter());
	}
}
