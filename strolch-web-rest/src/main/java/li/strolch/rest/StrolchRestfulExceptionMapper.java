/*
 * Copyright (c) 2015-2025 Robert von Burg <eitch@eitchnet.ch>
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

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.NotFoundException;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.exception.StrolchUserMessageException;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.helper.RestfulHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ResourceBundle;

import static li.strolch.model.Tags.AGENT;
import static li.strolch.privilege.model.CertificateThreadLocal.getCert;
import static li.strolch.privilege.model.CertificateThreadLocal.hasCert;
import static li.strolch.utils.helper.ExceptionHelper.hasCause;

@Provider
public class StrolchRestfulExceptionMapper implements ExceptionMapper<Exception> {

	private static final Logger logger = LoggerFactory.getLogger(StrolchRestfulExceptionMapper.class);

	@Context
	private HttpServletRequest request;

	@Override
	public Response toResponse(Exception ex) {

		logger.error("Handling exception {}", ex.getClass(), ex);

		StrolchAgent agent = RestfulStrolchComponent.getInstance().getAgent();
		boolean isNotAccessDeniedException = !hasCause(ex, AccessDeniedException.class) && !hasCause(ex,
				StrolchAccessDeniedException.class);
		boolean logException = isNotAccessDeniedException
				&& !hasCause(ex, StrolchUserMessageException.class)
				&& agent.hasComponent(OperationsLog.class);
		if (logException) {
			try {
				String username = hasCert() ? getCert().getUsername() : "anonymous";
				String realm = agent.getRealmNames().iterator().next();
				OperationsLog operationsLog = agent.getComponent(OperationsLog.class);
				operationsLog.addMessage(new LogMessage(realm, username,
						Locator.valueOf(AGENT, RestfulStrolchComponent.class.getSimpleName(),
								ex.getClass().getSimpleName()), LogSeverity.Exception, LogMessageState.Information,
						ResourceBundle.getBundle("strolch-agent"), "web.rest.exception")
						.value("exception", ex)
						.value("method", this.request == null ? "??" : this.request.getMethod())
						.value("url", this.request == null ? "??" : this.request.getRequestURL().toString())
						.value("remoteIp", this.request == null ? "??" : RestfulHelper.getRemoteIp(this.request)));
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