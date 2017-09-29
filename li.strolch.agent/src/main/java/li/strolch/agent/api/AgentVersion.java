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

import com.google.gson.JsonObject;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AgentVersion extends StrolchVersion {

	public static final String AGENT_NAME = "agentName";

	private String agentName;

	public AgentVersion(String agentName, Properties properties) {
		super(properties);
		this.agentName = agentName;
	}

	public String getAgentName() {
		return this.agentName;
	}

	public void setAgentName(String agentName) {
		this.agentName = agentName;
	}

	@Override
	public JsonObject toJson() {
		JsonObject jsonObject = super.toJson();

		jsonObject.addProperty(AGENT_NAME, this.agentName);

		return jsonObject;
	}

	@Override
	public String toString() {
		return "AgentVersion{agentName='" + this.agentName + "' , groupId='" + getGroupId() + "' , artifactId='"
				+ getArtifactId() + "' , artifactVersion='" + getArtifactVersion() + "' , scmRevision='"
				+ getScmRevision() + "' , scmBranch='" + getScmBranch() + "' , buildTimestamp='" + getBuildTimestamp()
				+ "' }";
	}
}
