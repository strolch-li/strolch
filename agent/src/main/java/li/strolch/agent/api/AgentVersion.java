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

import static li.strolch.model.Tags.Json.*;

import java.util.Locale;
import java.util.Properties;

import com.google.gson.JsonObject;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AgentVersion extends StrolchVersion {

	private final String environment;
	private final String locale;
	private final String timezone;

	private String agentName;

	public AgentVersion(String agentName, String environment, Locale locale, String timezone, Properties properties) {
		super(properties);
		this.agentName = agentName;
		this.environment = environment;
		this.locale = locale.toLanguageTag();
		this.timezone = timezone;
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
		jsonObject.addProperty(ENVIRONMENT, this.environment);
		jsonObject.addProperty(LOCALE, this.locale);
		jsonObject.addProperty(TIMEZONE, this.timezone);

		return jsonObject;
	}

	@Override
	public String toString() {
		return "AgentVersion{agentName='" + this.agentName + "' , environment='" + this.environment + "' , locale='"
				+ this.locale + "' , timezone='" + this.timezone + "' , groupId='" + getGroupId() + "' , artifactId='"
				+ getArtifactId() + "' , artifactVersion='" + getArtifactVersion() + "' , scmRevision='"
				+ getScmRevision() + "' , scmBranch='" + getScmBranch() + "' , buildTimestamp='" + getBuildTimestamp()
				+ "' }";
	}
}
