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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "VersionQueryResult")
public class VersionQueryResult {

	@XmlElement(name = "agentVersion", type = AgentVersion.class)
	private AgentVersion agentVersion;
	@XmlElement(name = "componentVersions", type = ComponentVersion.class)
	private List<ComponentVersion> componentVersions;
	@XmlElement(name = "errors", type = String.class)
	private List<String> errors;

	public VersionQueryResult() {
		// no-arg constructor for JAXB
	}

	/**
	 * @return the agentVersion
	 */
	public AgentVersion getAgentVersion() {
		return this.agentVersion;
	}

	/**
	 * @param agentVersion
	 *            the agentVersion to set
	 */
	public void setAgentVersion(AgentVersion agentVersion) {
		this.agentVersion = agentVersion;
	}

	public void add(ComponentVersion version) {
		if (this.componentVersions == null) {
			this.componentVersions = new ArrayList<>();
		}
		this.componentVersions.add(version);
	}

	public void add(String error) {
		if (this.errors == null) {
			this.errors = new ArrayList<>();
		}
		this.errors.add(error);
	}

	/**
	 * @return the componentVersions
	 */
	public List<ComponentVersion> getComponentVersions() {
		return this.componentVersions;
	}

	/**
	 * @param componentVersions
	 *            the componentVersions to set
	 */
	public void setComponentVersions(List<ComponentVersion> componentVersions) {
		this.componentVersions = componentVersions;
	}

	/**
	 * @return the errors
	 */
	public List<String> getErrors() {
		return this.errors;
	}

	/**
	 * @param errors
	 *            the errors to set
	 */
	public void setErrors(List<String> errors) {
		this.errors = errors;
	}

	/**
	 * @return
	 */
	public boolean hasErrors() {
		return this.errors != null && !this.errors.isEmpty();
	}
}
