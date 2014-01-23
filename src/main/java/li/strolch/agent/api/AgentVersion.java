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

import java.util.Properties;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "AgentVersion")
public class AgentVersion extends AbstractVersion {

	@XmlAttribute(name = "agentName")
	private String agentName;

	public AgentVersion() {
		// no-arg constructor for JAXB
	}

	/**
	 * @param agentName
	 * @param properties
	 */
	public AgentVersion(String agentName, Properties properties) {
		super(properties);
		this.agentName = agentName;
	}

	/**
	 * @return the agentName
	 */
	public String getAgentName() {
		return this.agentName;
	}

	/**
	 * @param agentName
	 *            the agentName to set
	 */
	public void setAgentName(String agentName) {
		this.agentName = agentName;
	}

	@Override
	public String toString() {
		return "AgentVersion [agentName=" + this.agentName + ", groupId=" + this.getGroupId() + ", artifactId="
				+ this.getArtifactId() + ", artifactVersion=" + this.getArtifactVersion() + ", scmRevision="
				+ this.getScmRevision() + ", scmBranch=" + this.getScmBranch() + ", buildTimestamp="
				+ this.getBuildTimestamp() + "]";
	}
}
