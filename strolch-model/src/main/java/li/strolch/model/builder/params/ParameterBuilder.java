/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.model.parameter.Parameter;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

public abstract class ParameterBuilder<R, S extends Parameter<R>, T extends ParameterBagContainerBuilder<T>> {

	private final BagBuilder<T> builder;

	protected final String id;
	protected final String name;

	protected boolean hidden = false;
	protected int index;
	protected String interpretation = INTERPRETATION_NONE;
	protected String uom = UOM_NONE;

	protected R value;

	public ParameterBuilder(BagBuilder<T> builder, String id, String name) {
		this.builder = builder;
		this.id = id;
		this.name = name;
	}

	public ParameterBuilder<R, S, T> hidden(boolean hidden) {
		this.hidden = hidden;
		return this;
	}

	public ParameterBuilder<R, S, T> index(int index) {
		this.index = index;
		return this;
	}

	public ParameterBuilder<R, S, T> interpretation(String interpretation) {
		this.interpretation = interpretation;
		return this;
	}

	public ParameterBuilder<R, S, T> uom(String uom) {
		this.uom = uom;
		return this;
	}

	public ParameterBuilder<R, S, T> value(R value) {
		this.value = value;
		return this;
	}

	public BagBuilder<T> end() {
		return this.builder;
	}

	protected S applyParameter(S parameter) {
		parameter.setId(this.id);
		parameter.setName(this.name);
		parameter.setHidden(this.hidden);
		parameter.setIndex(this.index);
		parameter.setInterpretation(this.interpretation);
		parameter.setUom(this.uom);
		if (this.value != null)
			parameter.setValue(this.value);
		else
			parameter.clear();
		return parameter;
	}

	public abstract S build();
}
