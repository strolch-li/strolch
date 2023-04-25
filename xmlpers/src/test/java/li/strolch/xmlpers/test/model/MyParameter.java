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
package li.strolch.xmlpers.test.model;

public class MyParameter {

	private String id;
	private String name;
	private String type;
	private String value;

	/**
	 *
	 */
	public MyParameter() {
		// empty constructor
	}

	/**
	 * @param id
	 * @param name
	 * @param type
	 * @param value
	 */
	public MyParameter(String id, String name, String type, String value) {
		super();
		this.id = id;
		this.name = name;
		this.type = type;
		this.value = value;
	}

	@Override
	public String toString() {
		return "Parameter [id=" + this.id + ", name=" + this.name + ", type=" + this.type + ", value=" + this.value
				+ "]";
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * @param id
	 * 		the id to set
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
	 * 		the name to set
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
	 * 		the type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * @return the value
	 */
	public String getValue() {
		return this.value;
	}

	/**
	 * @param value
	 * 		the value to set
	 */
	public void setValue(String value) {
		this.value = value;
	}
}