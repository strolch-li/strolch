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

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.model.json.PrivilegeElementFromJsonVisitor;
import li.strolch.model.json.PrivilegeElementToJsonVisitor;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.CreatePersonalAccessTokenArgument;
import li.strolch.privilege.model.PersonalAccessTokenRep;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.ServiceResultResponse;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.privilege.tokens.*;
import li.strolch.service.privilege.users.PrivilegeUserIdArgument;

import java.util.List;

import static li.strolch.rest.helper.ResponseUtil.toResponse;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/tokens")
@Tag(name = "Personal Access Tokens", description = "Endpoints for managing personal access tokens.")
public class PersonalAccessTokenResource {

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[1];
		return element.getClassName() + "." + element.getMethodName();
	}

	@Operation(summary = "Get all personal access tokens",
			description = "Retrieves a list of all personal access tokens for the authenticated user.", responses = {
			@ApiResponse(responseCode = "200", description = "Tokens retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "array"))),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getTokens(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		GetPersonalAccessTokensService svc = new GetPersonalAccessTokensService();

		PrivilegeTokenResult svcResult = svcHandler.doService(cert, svc, svc.getArgumentInstance());
		if (svcResult.isNok())
			return toResponse(svcResult);

		List<PersonalAccessTokenRep> tokens = svcResult.getTokens();
		PrivilegeElementToJsonVisitor visitor = new PrivilegeElementToJsonVisitor();
		JsonArray result = tokens.stream().map(t -> t.accept(visitor)).collect(JsonArray::new, JsonArray::add,
				JsonArray::addAll);

		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		return Response.ok(gson.toJson(result), MediaType.APPLICATION_JSON).build();
	}

	@Operation(summary = "Create a new personal access token", description = "Creates a new personal access token.",
			responses = {@ApiResponse(responseCode = "200", description = "Token created successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "string"))),
					@ApiResponse(responseCode = "400", description = "Invalid request format."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response createToken(String data, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		CreatePersonalAccessTokenArgument arg = new PrivilegeElementFromJsonVisitor()
				.createPersonalAccessTokenArgumentFromJson(data);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		CreatePersonalAccessTokenService svc = new CreatePersonalAccessTokenService();
		CreatePersonalAccessTokenService.CreatePersonalAccessTokenServiceArgument svcArg = svc.getArgumentInstance();
		svcArg.arg = arg;

		PrivilegeTokenResult svcResult = svcHandler.doService(cert, svc, svcArg);
		if (svcResult.isNok())
			return toResponse(svcResult);

		return Response.ok(svcResult.getRawToken(), MediaType.TEXT_PLAIN).build();
	}

	@Operation(summary = "Remove a personal access token",
			description = "Deletes a personal access token by its ID", responses = {
			@ApiResponse(responseCode = "200", description = "Token removed successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "404", description = "Token not found."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{tokenId}")
	public Response removeToken(@PathParam("tokenId") String tokenId, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ServiceHandler svcHandler = RestfulStrolchComponent.getInstance().getComponent(ServiceHandler.class);
		RemovePersonalAccessTokenService svc = new RemovePersonalAccessTokenService();
		PrivilegeUserIdArgument arg = svc.getArgumentInstance();
		arg.userId = tokenId;

		ServiceResult svcResult = svcHandler.doService(cert, svc, arg);
		return toResponse(svcResult);
	}
}
