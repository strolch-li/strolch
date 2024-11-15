/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.model.parameter.DateParameter;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Date;

public class DateParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<Date, DateParameter, T> {

	private ZonedDateTime valueZdt;
	private LocalDateTime valueLdt;

	public DateParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	public ParameterBuilder<Date, DateParameter, T> value(ZonedDateTime value) {
		this.valueZdt = value;
		return this;
	}

	public ParameterBuilder<Date, DateParameter, T> value(LocalDateTime value) {
		this.valueLdt = value;
		return this;
	}

	@Override
	public DateParameter build() {
		DateParameter parameter = new DateParameter();
		applyParameter(parameter);
		if (this.valueZdt != null)
			parameter.setValueFromZonedDateTime(this.valueZdt);
		if (this.valueLdt != null)
			parameter.setValueFromLocalDateTime(this.valueLdt);
		return parameter;
	}
}
