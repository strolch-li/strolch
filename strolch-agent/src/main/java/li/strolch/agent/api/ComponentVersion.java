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

import com.google.gson.JsonObject;

import java.util.Properties;

import static java.text.MessageFormat.format;
import static li.strolch.model.Tags.Json.COMPONENT_NAME;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ComponentVersion extends StrolchVersion {

	private String componentName;

	public ComponentVersion(String componentName, Properties properties) {
		super(properties);
		this.componentName = componentName;
	}

	public String getComponentName() {
		return this.componentName;
	}

	public void setComponentName(String componentName) {
		this.componentName = componentName;
	}

	@Override
	public JsonObject toJson(boolean isAdminRequest, boolean withVersion) {
		JsonObject jsonObject = super.toJson(isAdminRequest, withVersion);
		jsonObject.addProperty(COMPONENT_NAME, this.componentName);
		return jsonObject;
	}

	@Override
	public String toString() {
		return format(
				"ComponentVersion'{'componentName=''{0}'' , groupId=''{1}'' , artifactId=''{2}'' , artifactVersion=''{3}'' , scmRevision=''{4}'' , scmBranch=''{5}'' , buildTimestamp=''{6}'' '}'",
				this.componentName, getGroupId(), getArtifactId(), getArtifactVersion(), getScmRevision(),
				getScmBranch(), getBuildTimestamp());
	}
}
