package li.strolch.rest.endpoint;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

import static li.strolch.model.Tags.Json.COMPONENTS;
import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;
import static li.strolch.utils.helper.ExceptionHelper.getCallerMethodNoClass;

@Path("strolch/agent")
public class AgentResource {

	private static final Logger logger = LoggerFactory.getLogger(AgentResource.class);

	private static Certificate validateCertificate(HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();
		rest.validate(cert).validateAction(Tags.AGENT, getCallerMethodNoClass(2));
		return cert;
	}

	@PUT
	@Path("configuration/reload")
	@Produces(MediaType.APPLICATION_JSON)
	public Response reloadConfiguration(@Context HttpServletRequest request) {
		validateCertificate(request);

		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();
		rest.getAgent().reloadStrolchConfiguration();

		return ResponseUtil.toResponse();
	}

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
			logger.error("Failed to change state of component " + name + " to state " + state, e);
			return ResponseUtil.toResponse(e);
		}

		return ResponseUtil.toResponse();
	}
}
