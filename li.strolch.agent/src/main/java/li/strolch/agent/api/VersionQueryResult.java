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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class VersionQueryResult {

	private StrolchVersion appVersion;
	private AgentVersion agentVersion;
	private List<ComponentVersion> componentVersions;
	private List<String> errors;

	public StrolchVersion getAppVersion() {
		return this.appVersion;
	}

	public void setAppVersion(StrolchVersion appVersion) {
		this.appVersion = appVersion;
	}

	public AgentVersion getAgentVersion() {
		return this.agentVersion;
	}

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

	public List<ComponentVersion> getComponentVersions() {
		return this.componentVersions;
	}

	public void setComponentVersions(List<ComponentVersion> componentVersions) {
		this.componentVersions = componentVersions;
	}

	public List<String> getErrors() {
		return this.errors;
	}

	public void setErrors(List<String> errors) {
		this.errors = errors;
	}

	public boolean hasErrors() {
		return this.errors != null && !this.errors.isEmpty();
	}

	public JsonObject toJson() {
		JsonObject jsonObject = new JsonObject();

		jsonObject.add("appVersion", this.appVersion.toJson());
		jsonObject.add("agentVersion", this.agentVersion.toJson());

		JsonArray componentVersionsJ = new JsonArray();
		this.componentVersions.forEach(c -> componentVersionsJ.add(c.toJson()));
		jsonObject.add("componentVersions", componentVersionsJ);

		if (this.errors != null) {
			JsonArray errorsJ = new JsonArray();
			this.errors.forEach(errorsJ::add);
			jsonObject.add("errors", errorsJ);
		}

		return jsonObject;
	}
}
