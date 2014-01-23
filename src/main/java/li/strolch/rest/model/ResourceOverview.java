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
package li.strolch.rest.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import li.strolch.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "Resource")
public class ResourceOverview extends StrolchElementOverview {

	public ResourceOverview() {
		// no-arg constructor for JAXB
	}

	/**
	 * @param id
	 * @param name
	 * @param type
	 */
	public ResourceOverview(String id, String name, String type) {
		super(id, name, type);
	}

	/**
	 * @param resource
	 */
	public ResourceOverview(Resource resource) {
		super(resource);
	}
}
