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
package ch.eitchnet.xmlpers.test.model;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

public class Resource {

	private String id;
	private String name;
	private String type;
	private Map<String, Parameter> parameters = new HashMap<String, Parameter>();

	/**
	 *
	 */
	public Resource() {
		// empty constructor
	}

	/**
	 * @param id
	 * @param name
	 * @param type
	 */
	public Resource(String id, String name, String type) {
		super();
		this.id = id;
		this.name = name;
		this.type = type;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Resource [id=");
		builder.append(this.id);
		builder.append(", name=");
		builder.append(this.name);
		builder.append(", type=");
		builder.append(this.type);
		builder.append(", parameters=");
		for (Entry<String, Parameter> param : this.parameters.entrySet()) {
			builder.append("\n");
			builder.append("  " + param.getKey() + " = " + param.getValue());
		}
		builder.append("]");
		return builder.toString();
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the type
	 */
	public String getType() {
		return this.type;
	}

	/**
	 * @param type the type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	public void addParameter(Parameter parameter) {
		this.parameters.put(parameter.getId(), parameter);
	}

	public Set<String> getParameterKeySet() {
		return this.parameters.keySet();
	}

	public Parameter getParameterBy(String id) {
		return this.parameters.get(id);
	}
}
