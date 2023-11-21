package li.strolch.rest.endpoint;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
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

import java.util.Set;

import static li.strolch.utils.helper.ExceptionHelper.getRootCauseMessage;

@Path("strolch/languages")
public class LanguagesResource {

	private static final Logger logger = LoggerFactory.getLogger(LanguagesResource.class);

	@GET
	@Path("supported")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getSupportedLanguages() {
		try {
			Set<SupportedLanguage> supportedLanguages = RestfulStrolchComponent.getInstance().getAgent()
					.getStrolchConfiguration().getRuntimeConfiguration().getSupportedLanguages();
			JsonArray result = supportedLanguages.stream().map(language -> {
				JsonObject jsonObject = new JsonObject();
				jsonObject.addProperty(Tags.Json.LOCALE, language.locale());
				jsonObject.addProperty(Tags.Json.NAME, language.name());
				return jsonObject;
			}).collect(JsonArray::new, JsonArray::add, JsonArray::addAll);

			return Response.ok().entity(result.toString()).build();
		} catch (Exception e) {
			logger.error("Failed to get supported languages: " + e.getMessage(), e);
			return Response.serverError().entity(getRootCauseMessage(e)).build();
		}
	}
}
