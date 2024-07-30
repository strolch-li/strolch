package li.strolch.rest.helper;

import jakarta.ws.rs.core.Response;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.privilege.base.InvalidCredentialsException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Usage;
import li.strolch.runtime.sessions.StrolchSessionHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Base64;

import static java.nio.charset.StandardCharsets.UTF_8;
import static li.strolch.utils.helper.ExceptionHelper.getRootCause;
import static li.strolch.utils.helper.ExceptionHelper.getRootCauseExceptionMessage;
import static li.strolch.utils.helper.StringHelper.isEmpty;

public class BasicAuth {

	private static final Logger logger = LoggerFactory.getLogger(BasicAuth.class);

	private final StrolchSessionHandler sessionHandler;

	public BasicAuth(StrolchSessionHandler sessionHandler) {
		this.sessionHandler = sessionHandler;
	}

	public Certificate doBasicAuth(String authorization, String remoteIp) throws BasicAuthFailure {
		if (isEmpty(authorization))
			throw new BasicAuthFailure(Response.Status.UNAUTHORIZED, "No authentication!");
		if (!authorization.startsWith("Basic "))
			throw new BasicAuthFailure(Response.Status.BAD_REQUEST, "Invalid basic auth request!");

		try {
			String auth = new String(Base64.getDecoder().decode(authorization.substring(6)), UTF_8);
			int splitIndex = auth.indexOf(':');
			String username = auth.substring(0, splitIndex);
			String password = auth.substring(splitIndex + 1);

			return this.sessionHandler.authenticate(username, password.toCharArray(), remoteIp, Usage.SINGLE, false);

		} catch (StrolchNotAuthenticatedException | InvalidCredentialsException e) {
			logger.error(e.getMessage());
			throw new BasicAuthFailure(Response.Status.UNAUTHORIZED, "Authentication failed", e);
		} catch (StrolchAccessDeniedException e) {
			logger.error(e.getMessage());
			throw new BasicAuthFailure(Response.Status.FORBIDDEN, "User is not authorized!", e);
		} catch (Exception e) {
			Response.Status status;
			String msg;
			Throwable rootCause = getRootCause(e);
			if (rootCause instanceof StrolchNotAuthenticatedException
					|| rootCause instanceof InvalidCredentialsException) {
				status = Response.Status.UNAUTHORIZED;
				msg = "Authentication failed";
			} else if (rootCause instanceof StrolchAccessDeniedException) {
				status = Response.Status.FORBIDDEN;
				msg = "User is not authorized!";
			} else {
				status = Response.Status.INTERNAL_SERVER_ERROR;
				msg = "Internal error";
			}

			if (status == Response.Status.INTERNAL_SERVER_ERROR)
				logger.error(e.getMessage(), e);
			else
				logger.error("Basic Auth failed: {}", getRootCauseExceptionMessage(e));
			throw new BasicAuthFailure(status, msg, e);
		}
	}
}
