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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.model.Tags;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.runtime.configuration.SupportedLanguage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static java.util.Comparator.comparing;
import static li.strolch.utils.helper.ExceptionHelper.getRootCauseMessage;

@Path("strolch/languages")
@Tag(name = "I18n", description = "Provides operations for inspecting the agent's supported languages")
public class LanguagesResource {

	private static final Logger logger = LoggerFactory.getLogger(LanguagesResource.class);

	@Operation(summary = "Get supported languages",
			description = "Retrieves a list of supported languages available in the system.")
	@ApiResponse(responseCode = "200", description = "List of supported languages retrieved successfully.",
			content = @Content(mediaType = "application/json", array = @ArraySchema(schema = @Schema(type = "object"))))
	@ApiResponse(responseCode = "500", description = "Internal server error.")
	@GET
	@Path("supported")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getSupportedLanguages() {
		try {
			JsonArray result = RestfulStrolchComponent
					.getInstance()
					.getAgent()
					.getRuntimeConfiguration()
					.getSupportedLanguages()
					.stream()
					.sorted(comparing(SupportedLanguage::name))
					.map(language -> {
						JsonObject jsonObject = new JsonObject();
						jsonObject.addProperty(Tags.Json.LOCALE, language.locale());
						jsonObject.addProperty(Tags.Json.NAME, language.name());
						return jsonObject;
					})
					.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);

			return Response.ok().entity(result.toString()).build();
		} catch (Exception e) {
			logger.error("Failed to get supported languages: {}", e.getMessage(), e);
			return Response.serverError().entity(getRootCauseMessage(e)).build();
		}
	}
}
