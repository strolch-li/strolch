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
package li.strolch.agent.api;

import li.strolch.privilege.model.Restrictable;

/**
 * A simple implementation for the {@link Restrictable} interface
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RestrictableElement implements Restrictable {

	private final String name;
	private final Object value;

	public RestrictableElement(String name, Object value) {
		super();
		this.name = name;
		this.value = value;
	}

	@Override
	public String getPrivilegeName() {
		return this.name;
	}

	@Override
	public Object getPrivilegeValue() {
		return this.value;
	}

	public static Restrictable restrictableFor(String name, Object value) {
		return new RestrictableElement(name, value);
	}
}
