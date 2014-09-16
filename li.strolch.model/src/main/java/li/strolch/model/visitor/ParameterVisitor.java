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
package li.strolch.model.visitor;

import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.ListParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ParameterVisitor {

	public <T> T visitParam(Parameter<?> param);

	public <T> T visitBooleanParam(BooleanParameter param);

	public <T> T visitDateParam(DateParameter param);

	public <T> T visitFloatParam(FloatParameter param);

	public <T> T visitIntegerParam(IntegerParameter param);

	public <T> T visitLongParam(LongParameter param);

	public <T> T visitStringParam(StringParameter param);

	public <T> T visitListParam(ListParameter<?> param);

	public <T> T visitStringListParam(StringListParameter param);
}
