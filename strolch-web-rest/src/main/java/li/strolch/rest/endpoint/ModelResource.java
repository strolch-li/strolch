/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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
import com.google.gson.JsonParser;
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
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.soql.core.QueryProcessor;
import li.strolch.soql.core.QueryRequest;
import li.strolch.soql.core.QueryResponse;

@Path("strolch/model")
@Tag(name = "Model", description = "Endpoints for querying and managing Strolch models.")
public class ModelResource {

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[2];
		return element.getClassName() + "." + element.getMethodName();
	}

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().openTx(certificate, realm, getContext());
	}

	@Operation(summary = "Execute SOQL query",
			description = "Processes a Strolch Object Query Language (SOQL) request and returns the result.",
			responses = {@ApiResponse(responseCode = "200", description = "Query executed successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "400", description = "Invalid query format."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Path("soql")
	public Response doQuery(@Context HttpServletRequest request, @QueryParam("realmName") String realmName,
			@QueryParam("flat") @DefaultValue("false") boolean flat, String data) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();

		QueryResponse queryResponse;
		try (StrolchTransaction tx = openTx(cert, realmName)) {
			QueryRequest queryRequest = QueryRequest.fromJson(jsonObject);
			QueryProcessor queryProcessor = new QueryProcessor();
			queryResponse = queryProcessor.process(queryRequest, tx);
		}

		return Response.ok(queryResponse.asJson(flat).toString(), MediaType.APPLICATION_JSON).build();
	}
}
