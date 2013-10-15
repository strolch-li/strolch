/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
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
	 * @param id
	 *            the id to set
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
	 * @param name
	 *            the name to set
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
	 * @param type
	 *            the type to set
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