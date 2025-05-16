/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.ExternalDocumentation;
import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.info.Contact;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.info.License;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.agent.api.ComponentState;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.Tags;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.model.StrolchResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

import static li.strolch.model.Tags.Json.COMPONENTS;
import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;
import static li.strolch.utils.helper.ExceptionHelper.getCallerMethodNoClass;

@Path("strolch/agent")
@OpenAPIDefinition(info = @Info(title = "Strolch RESTful API", version = "1.0", description = """
		Strolch is an agent implementing a service architecture. With the exception of JAX-RS, everything is implemented in Java SE.
		
		All aspects of a framework for applications are implemented:
		- Parameterized model
		- Transaction management
		- Privilege management
		- Services and Commands for Use-Cases
		- Policies
		- Component lifecycle management""",
		summary = "This API implements all aspects of modifying the agent at runtime: Component lifecycle management, Authentication, Execution control, Inspector for data and configuration, Job control, Notification control, Privilege management",
		license = @License(name = "Apache 2.0", url = "https://strolch.li"),
		contact = @Contact(url = "https://www.eitchnet.ch", name = "Robert von Burg", email = "eitch@eitchnet.ch")),
		externalDocs = @ExternalDocumentation(description = "Strolch's website", url = "https://strolch.li"))
@Tag(name = "Agent", description = "API to view and modify components and configuration")
public class AgentResource {

	private static final Logger logger = LoggerFactory.getLogger(AgentResource.class);

	private static void validateCertificate(HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();
		rest.validate(cert).validateAction(Tags.AGENT, getCallerMethodNoClass(2));
	}

	@Operation(summary = "Get agent statistics", description = "Retrieves runtime statistics of the agent.",
			responses = {@ApiResponse(responseCode = "200", description = "Agent statistics retrieved.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = StrolchResponse.class))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Path("statistics")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getStatistics(@Context HttpServletRequest request) {
		validateCertificate(request);
		StrolchAgent agent = RestfulStrolchComponent.getInstance().getAgent();
		JsonObject statisticsJson = agent.getAgentStatistics().toJson();
		return ResponseUtil.toResponse(DATA, statisticsJson);
	}

	@Operation(summary = "Reload agent configuration", description = "Forces the agent to reload its configuration.",
			responses = {@ApiResponse(responseCode = "200", description = "Configuration reloaded successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = StrolchResponse.class))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Path("configuration/reload")
	@Produces(MediaType.APPLICATION_JSON)
	public Response reloadConfiguration(@Context HttpServletRequest request) {
		validateCertificate(request);

		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();
		rest.getAgent().reloadStrolchConfiguration();

		return ResponseUtil.toResponse();
	}

	@Operation(summary = "Get agent components", description = "Retrieves all components registered in the agent.",
			responses = {@ApiResponse(responseCode = "200", description = "List of agent components.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = StrolchResponse.class))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Path("components")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getComponents(@Context HttpServletRequest request) {
		validateCertificate(request);
		StrolchAgent agent = RestfulStrolchComponent.getInstance().getAgent();
		List<StrolchComponent> components = agent.getComponentsOrderedByRoot();

		JsonObject resultJ = new JsonObject();
		resultJ.add(Tags.Json.AGENT, agent.toJson());
		JsonArray componentsJ = new JsonArray();
		for (StrolchComponent component : components) {
			componentsJ.add(component.toJson());
		}
		resultJ.add(COMPONENTS, componentsJ);

		return ResponseUtil.toResponse(DATA, resultJ);
	}

	@Operation(summary = "Set component state", description = "Updates the state of a specific component.",
			responses = {@ApiResponse(responseCode = "200", description = "Component state updated successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = StrolchResponse.class))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "404", description = "Component not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Path("components/{name}/state")
	@Produces(MediaType.APPLICATION_JSON)
	public Response setComponentState(@Context HttpServletRequest request, @PathParam("name") String name,
			@QueryParam("state") String newState) {
		ComponentState state = ComponentState.valueOf(newState);
		validateCertificate(request);

		StrolchComponent component = RestfulStrolchComponent.getInstance().getComponentByName(name);
		try {
			switch (state) {
				case INITIALIZED -> {
					if (component.getState() == ComponentState.DESTROYED)
						component.setup(component.getConfiguration());
					component.initialize(component.getConfiguration());
				}
				case STARTED -> component.start();
				case STOPPED -> component.stop();
				case DESTROYED -> component.destroy();
				default -> throw new IllegalStateException("Unhandled state " + state);
			}
		} catch (Exception e) {
			logger.error("Failed to change state of component {} to state {}", name, state, e);
			return ResponseUtil.toResponse(e);
		}

		return ResponseUtil.toResponse();
	}
}
