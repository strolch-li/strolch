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
package li.strolch.model.timedstate;

import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("rawtypes")
public interface StrolchTimedState<T extends IValue> extends StrolchElement {

	public ITimeValue<T> getNextMatch(Long time, T value);

	public ITimeValue<T> getPreviousMatch(Long time, T value);

	public <U extends IValueChange<T>> void applyChange(U change);

	public ITimeValue<T> getStateAt(Long time);

	public ITimeVariable<T> getTimeEvolution();

	public void setParent(Resource aThis);
}
