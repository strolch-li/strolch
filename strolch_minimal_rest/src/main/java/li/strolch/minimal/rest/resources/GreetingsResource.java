package li.strolch.minimal.rest.resources;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import li.strolch.minimal.rest.util.Result;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

@Path("greetings")
public class GreetingsResource {

	private static final Logger logger = LoggerFactory.getLogger(GreetingsResource.class);

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response helloWorld(@QueryParam("name") String name) {

		logger.info("Received request for: " + name);

		Result result = new Result("Hello " + name);
		String entity = new Gson().toJson(result);

		return Response.ok(entity, MediaType.APPLICATION_JSON).build();
	}
}
