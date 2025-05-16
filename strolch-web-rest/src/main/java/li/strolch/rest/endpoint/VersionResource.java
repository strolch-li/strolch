/*
 * Copyright (c) 2015-2025 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.VersionQueryResult;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;

import static li.strolch.model.StrolchModelConstants.ROLE_STROLCH_ADMIN;
import static li.strolch.model.Tags.Json.AGENT_VERSION;
import static li.strolch.model.Tags.Json.APP_VERSION;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/version")
@Tag(name = "Version", description = "Retrieves version information")
public class VersionResource {

	@Operation(summary = "Get system versions",
			description = "Retrieves the agent and application version information.", responses = {
			@ApiResponse(responseCode = "200", description = "Successfully retrieved version information",
					content = @Content(mediaType = "application/json")),
			@ApiResponse(responseCode = "500", description = "Internal server error")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getVersions(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		StrolchAgent agent = RestfulStrolchComponent.getInstance().getAgent();

		VersionQueryResult versionQuery = agent.getVersion();
		if (cert == null) {
			JsonObject jsonObject = new JsonObject();

			boolean hideVersion = RestfulStrolchComponent.getInstance().isHideVersionFromUnauthorizedClients();
			jsonObject.add(AGENT_VERSION, versionQuery.getAgentVersion().toJson(false, !hideVersion));
			jsonObject.add(APP_VERSION, versionQuery.getAppVersion().toJson(false, !hideVersion));

			return Response.ok(jsonObject.toString(), MediaType.APPLICATION_JSON).build();
		}

		boolean isStrolchAdmin = cert.hasRole(ROLE_STROLCH_ADMIN);
		return Response.ok(versionQuery.toJson(isStrolchAdmin, true).toString(), MediaType.APPLICATION_JSON).build();
	}
}
