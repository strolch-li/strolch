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
package li.strolch.rest.inspector.test;

import static org.junit.Assert.assertEquals;

import java.util.List;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import li.strolch.agent.api.AgentVersion;
import li.strolch.agent.api.ComponentVersion;
import li.strolch.agent.api.VersionQueryResult;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class VersionQueryTest extends AbstractRestfulTest {

	private static final String ROOT_PATH = "strolch/version";

	@Test
	public void shouldQueryVersion() {

		// query
		Response result = target().path(ROOT_PATH).request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		VersionQueryResult versionQueryResult = result.readEntity(VersionQueryResult.class);
		if (versionQueryResult.hasErrors()) {
			for (String error : versionQueryResult.getErrors()) {
				logger.error(error);
			}
		}

		AgentVersion agentVersion = versionQueryResult.getAgentVersion();
		logger.info(agentVersion.toString());
		List<ComponentVersion> componentVersions = versionQueryResult.getComponentVersions();
		assertEquals(6, componentVersions.size());
		for (ComponentVersion version : componentVersions) {
			logger.info(version.toString());
			assertEquals("li.strolch", agentVersion.getGroupId());
		}

		assertEquals("StrolchPersistenceTest", agentVersion.getAgentName());
		assertEquals("li.strolch", agentVersion.getGroupId());
		assertEquals("li.strolch.agent", agentVersion.getArtifactId());
	}
}
