package li.strolch.minimal.rest.util;

import java.text.MessageFormat;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

@Provider
public class RestfulExceptionMapper implements ExceptionMapper<Exception> {

	private static final Logger logger = LoggerFactory.getLogger(RestfulExceptionMapper.class);

	@Override
	public Response toResponse(Exception ex) {
		logger.error(MessageFormat.format("Handling exception {0}", ex.getClass()), ex); //$NON-NLS-1$
		return Response.serverError().entity(new Gson().toJson(new Result(ex))).type(MediaType.APPLICATION_JSON)
				.build();
	}
}