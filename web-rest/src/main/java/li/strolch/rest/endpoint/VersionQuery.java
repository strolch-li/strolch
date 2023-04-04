/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.rest.endpoint;

import static li.strolch.model.Tags.Json.*;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/version")
public class VersionQuery {

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getVersions(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		StrolchAgent agent = RestfulStrolchComponent.getInstance().getAgent();

		if (cert == null && RestfulStrolchComponent.getInstance().isHideVersionFromUnauthorizedClients()) {

			JsonObject agentVersion = new JsonObject();
			agentVersion.addProperty(AGENT_NAME, agent.getApplicationName());
			agentVersion.addProperty(ENVIRONMENT, agent.getEnvironment());
			agentVersion.addProperty(LOCALE, agent.getLocale().toLanguageTag());
			agentVersion.addProperty(TIMEZONE, agent.getTimezone());

			JsonArray componentVersionsJ = new JsonArray();
			agent.getVersion().getComponentVersions().forEach(c -> {
				JsonObject componentVersionJ = new JsonObject();
				componentVersionJ.addProperty(COMPONENT_NAME, c.getComponentName());
				componentVersionsJ.add(componentVersionJ);
			});

			JsonObject jsonObject = new JsonObject();
			jsonObject.add(AGENT_VERSION, agentVersion);
			jsonObject.add(APP_VERSION, new JsonObject());
			jsonObject.add(COMPONENT_VERSIONS, componentVersionsJ);

			return Response.ok(jsonObject.toString(), MediaType.APPLICATION_JSON).build();
		}

		return Response.ok(agent.getVersion().toJson().toString(), MediaType.APPLICATION_JSON).build();
	}
}
