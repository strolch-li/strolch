/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.rest;

import jakarta.ws.rs.NotFoundException;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.model.CertificateThreadLocal;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.utils.helper.ExceptionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ResourceBundle;

import static li.strolch.model.Tags.AGENT;
import static li.strolch.utils.helper.ExceptionHelper.*;

@Provider
public class StrolchRestfulExceptionMapper implements ExceptionMapper<Exception> {

	private static final Logger logger = LoggerFactory.getLogger(StrolchRestfulExceptionMapper.class);

	@Override
	public Response toResponse(Exception ex) {

		logger.error("Handling exception {}", ex.getClass(), ex);

		RestfulStrolchComponent instance = RestfulStrolchComponent.getInstance();
		boolean isNotAccessDeniedException = !hasCause(ex, AccessDeniedException.class) && !hasCause(ex,
				StrolchAccessDeniedException.class);
		if (isNotAccessDeniedException && instance.hasComponent(OperationsLog.class)) {
			try {
				String username = CertificateThreadLocal.hasCert() ? CertificateThreadLocal.getCert().getUsername() :
						"anonymous";
				String realm = instance.getAgent().getRealmNames().iterator().next();
				OperationsLog operationsLog = instance.getComponent(OperationsLog.class);
				operationsLog.addMessage(new LogMessage(realm, username,
						Locator.valueOf(AGENT, RestfulStrolchComponent.class.getSimpleName(),
								ex.getClass().getSimpleName()), LogSeverity.Exception, LogMessageState.Information,
						ResourceBundle.getBundle("strolch-agent"), "web.rest.exception")
						.withException(ex)
						.value("exception", ex));
			} catch (Exception e) {
				logger.error("Failed to add log message for exception!", e);
			}
		}

		return switch (ex) {
			case NotFoundException ignored -> ResponseUtil.toResponse(Status.NOT_FOUND, ex);
			case AccessDeniedException e -> ResponseUtil.toResponse(Status.FORBIDDEN, e.getMessage());
			case StrolchAccessDeniedException e -> ResponseUtil.toResponse(Status.FORBIDDEN, e.getI18n());
			case StrolchNotAuthenticatedException e -> {
				logger.error("User tried to access resource, but was not authenticated: {}", ex.getMessage());
				yield Response.status(Status.UNAUTHORIZED).entity(e.getMessage()).type(MediaType.TEXT_PLAIN).build();
			}
			default -> ResponseUtil.toResponse(ex);
		};
	}
}