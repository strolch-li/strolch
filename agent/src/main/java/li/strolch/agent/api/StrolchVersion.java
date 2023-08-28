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

import java.util.Properties;

import com.google.gson.JsonObject;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchVersion {

	private String groupId;
	private String artifactId;
	private String artifactVersion;
	private String scmRevision;
	private String scmBranch;
	private String buildTimestamp;

	public StrolchVersion(Properties properties) {
		this.groupId = properties.getProperty(GROUP_ID);
		this.artifactId = properties.getProperty(ARTIFACT_ID);
		this.artifactVersion = properties.getProperty(ARTIFACT_VERSION);
		this.scmRevision = properties.getProperty(SCM_REVISION);
		this.scmBranch = properties.getProperty(SCM_BRANCH);
		this.buildTimestamp = properties.getProperty(BUILD_TIMESTAMP);
	}

	public String getGroupId() {
		return this.groupId;
	}

	public void setGroupId(String groupId) {
		this.groupId = groupId;
	}

	public String getArtifactId() {
		return this.artifactId;
	}

	public void setArtifactId(String artifactId) {
		this.artifactId = artifactId;
	}

	public String getArtifactVersion() {
		return this.artifactVersion;
	}

	public void setArtifactVersion(String artifactVersion) {
		this.artifactVersion = artifactVersion;
	}

	public String getScmRevision() {
		return this.scmRevision;
	}

	public void setScmRevision(String scmRevision) {
		this.scmRevision = scmRevision;
	}

	public String getScmBranch() {
		return this.scmBranch;
	}

	public void setScmBranch(String scmBranch) {
		this.scmBranch = scmBranch;
	}

	public String getBuildTimestamp() {
		return this.buildTimestamp;
	}

	public void setBuildTimestamp(String buildTimestamp) {
		this.buildTimestamp = buildTimestamp;
	}

	public JsonObject toJson(boolean isAdminRequest) {
		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty(GROUP_ID, this.groupId);
		jsonObject.addProperty(ARTIFACT_ID, this.artifactId);
		jsonObject.addProperty(ARTIFACT_VERSION, this.artifactVersion);
		if (isAdminRequest) {
			jsonObject.addProperty(SCM_REVISION, this.scmRevision);
			jsonObject.addProperty(SCM_BRANCH, this.scmBranch);
			jsonObject.addProperty(BUILD_TIMESTAMP, this.buildTimestamp);
		}

		return jsonObject;
	}

	@Override
	public String toString() {
		return "StrolchVersion{groupId='" + groupId + "' , artifactId='" + artifactId + "', artifactVersion='" +
				artifactVersion + "' , scmRevision='" + scmRevision + "' , scmBranch='" + scmBranch +
				"' , buildTimestamp='" + buildTimestamp + "' }";
	}
}
