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
import li.strolch.model.timedstate.LongTimedState;
import li.strolch.model.timevalue.impl.LongValue;

public class LongStateBuilder extends TimedStateBuilder<LongTimedState> {

	public LongStateBuilder(ResourceBuilder builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public LongTimedState build() {
		LongTimedState state = new LongTimedState();
		applyTimedState(state);
		state.getTimeEvolution().setValueAt(0L, new LongValue(0L));
		return state;
	}
}
