/*
 * Copyright 2013 Martin Smock <smock.martin@gmail.com>
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
package li.strolch.model.activity;


/**
 * Traces the state of the {@link Action} in the sequence of creation, scheduling and planning. 
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
public enum ActionState {
	CREATED, 
	SCHEDULED, // when the start and end time is set and the value changes have been attached
	PLANNED; // when the value changes have been registered to the states of the resources
}
