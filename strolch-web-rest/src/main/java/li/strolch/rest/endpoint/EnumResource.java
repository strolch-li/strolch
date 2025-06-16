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

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.StringToClassMapItem;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.runtime.query.enums.EnumHandler;
import li.strolch.runtime.query.enums.StrolchEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Locale;

/**
 * Resource for retrieving enumerations.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/enums")
@Tag(name = "Enum Resource", description = "Provides operations to retrieve system enumerations.")
public class EnumResource {

	private static final Logger logger = LoggerFactory.getLogger(EnumResource.class);

	@Operation(summary = "Retrieve an enumeration",
			description = "Fetches an enumeration by name with optional hidden elements filtering.", responses = {
			@ApiResponse(responseCode = "200", description = "Successful retrieval of enumeration.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object",
							properties = {@StringToClassMapItem(key = "name", value = String.class),
									@StringToClassMapItem(key = "locale", value = String.class),
									@StringToClassMapItem(key = "values", value = Object.class)}))),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{name}")
	public Response getEnum(@Context HttpServletRequest request, @PathParam("name") String name,
			@QueryParam("withoutHidden") boolean withoutHidden) {
		try {
			EnumHandler enumHandler = RestfulStrolchComponent
					.getInstance()
					.getContainer()
					.getComponent(EnumHandler.class);

			Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
			StrolchEnum strolchEnum = enumHandler.getEnum(cert, name, cert.getLocale(), withoutHidden);
			return Response.ok().entity(strolchEnum.toJson().toString()).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return Response.serverError().entity(e.getMessage()).build();
		}
	}

	@Operation(summary = "Retrieve an enumeration with specific locale",
			description = "Fetches an enumeration by name with a specific locale override.", responses = {
			@ApiResponse(responseCode = "200", description = "Successful retrieval of enumeration with locale.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object",
							properties = {@StringToClassMapItem(key = "name", value = String.class),
									@StringToClassMapItem(key = "locale", value = String.class),
									@StringToClassMapItem(key = "values", value = Object.class)}))),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{name}/{locale}")
	public Response getEnumBySpecificLocale(@PathParam("name") String name, @PathParam("locale") String locale,
			@Context HttpServletRequest request) {
		try {
			EnumHandler enumHandler = RestfulStrolchComponent
					.getInstance()
					.getContainer()
					.getComponent(EnumHandler.class);

			Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
			StrolchEnum strolchEnum = enumHandler.getEnum(cert, name, Locale.forLanguageTag(locale));
			return Response.ok().entity(strolchEnum.toJson().toString()).build();
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return Response.serverError().entity(e.getMessage()).build();
		}
	}
}