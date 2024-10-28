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

package li.strolch.model.builder.states;

import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.timedstate.StrolchTimedState;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

public abstract class TimedStateBuilder<T extends StrolchTimedState<?>> {

	private final ResourceBuilder builder;
	protected final String id;
	protected final String name;

	protected boolean hidden = false;
	protected int index;
	protected String interpretation = INTERPRETATION_NONE;
	protected String uom = UOM_NONE;

	public TimedStateBuilder(ResourceBuilder builder, String id, String name) {
		this.builder = builder;
		this.id = id;
		this.name = name;
	}

	public TimedStateBuilder<T> hidden(boolean hidden) {
		this.hidden = hidden;
		return this;
	}

	public TimedStateBuilder<T> index(int index) {
		this.index = index;
		return this;
	}

	public TimedStateBuilder<T> interpretation(String interpretation) {
		this.interpretation = interpretation;
		return this;
	}

	public TimedStateBuilder<T> uom(String uom) {
		this.uom = uom;
		return this;
	}

	public ResourceBuilder end() {
		return this.builder;
	}

	public abstract T build();

	protected T applyTimedState(T state) {
		state.setId(this.id);
		state.setName(this.name);
		state.setHidden(this.hidden);
		state.setIndex(this.index);
		state.setInterpretation(this.interpretation);
		state.setUom(this.uom);
		return state;
	}
}
