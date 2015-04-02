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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import li.strolch.model.audit.AccessType;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "ActionSelection")
@XmlAccessorType(XmlAccessType.NONE)
public class ActionSelection {

	@XmlAttribute(name = "action")
	private String action;

	@XmlElement(name = "accessTypes")
	private List<AccessType> accessTypes;

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = action;
	}

	public List<AccessType> getAccessTypes() {
		return accessTypes;
	}

	public void setAccessTypes(List<AccessType> accessTypes) {
		this.accessTypes = accessTypes;
	}
}
