package li.strolch.rest;

import java.text.MessageFormat;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

import li.strolch.exception.StrolchAccessDeniedException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.utils.helper.StringHelper;

@Provider
public class StrolchRestfulExceptionMapper implements ExceptionMapper<Exception> {

	private static final Logger logger = LoggerFactory.getLogger(StrolchRestfulExceptionMapper.class);

	@Override
	public Response toResponse(Exception ex) {

		logger.error(MessageFormat.format("Handling exception {0}", ex.getClass()), ex); //$NON-NLS-1$

		if (ex instanceof StrolchAccessDeniedException) {
			StrolchAccessDeniedException e = (StrolchAccessDeniedException) ex;
			StringBuilder sb = new StringBuilder();
			sb.append("User ");
			sb.append(e.getCertificate().getUsername());
			sb.append(" does not have access to ");
			Restrictable restrictable = e.getRestrictable();
			if (restrictable == null) {
				sb.append(StringHelper.NULL);
			} else {
				sb.append(restrictable.getPrivilegeName());
				sb.append(" - ");
				sb.append(restrictable.getPrivilegeValue());
			}

			return Response.status(Status.FORBIDDEN).entity(sb.toString()).type(MediaType.TEXT_PLAIN).build();
		}

		String exceptionMessage = StringHelper.formatExceptionMessage(ex);
		return Response.status(Status.INTERNAL_SERVER_ERROR).entity(exceptionMessage).type(MediaType.TEXT_PLAIN)
				.build();
	}
}