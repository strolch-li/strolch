package li.strolch.rest.inspector;

import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

import org.glassfish.grizzly.utils.Exceptions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Provider
public class StrolchRestfulExceptionMapper implements ExceptionMapper<Exception> {

	private static final Logger logger = LoggerFactory.getLogger(StrolchRestfulExceptionMapper.class);

	@Override
	public Response toResponse(Exception ex) {
		logger.error("Handling exception " + ex.getClass(), ex);
		return Response.status(500).entity(Exceptions.getStackTraceAsString(ex)).type("text/plain").build();
	}
}