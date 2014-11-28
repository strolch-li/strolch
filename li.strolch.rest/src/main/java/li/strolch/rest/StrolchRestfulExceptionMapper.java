package li.strolch.rest;

import java.text.MessageFormat;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;

@Provider
public class StrolchRestfulExceptionMapper implements ExceptionMapper<Exception> {

	private static final Logger logger = LoggerFactory.getLogger(StrolchRestfulExceptionMapper.class);

	@Override
	public Response toResponse(Exception ex) {
		logger.error(MessageFormat.format("Handling exception {0}", ex.getClass()), ex); //$NON-NLS-1$
		return Response.status(500).entity(StringHelper.formatExceptionMessage(ex)).type(MediaType.TEXT_PLAIN).build();
	}
}