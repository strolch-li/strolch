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
package li.strolch.rest.endpoint;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.model.QueryData;
import li.strolch.runtime.sessions.StrolchSessionHandler;
import li.strolch.runtime.sessions.UserSession;
import li.strolch.search.SearchResult;
import li.strolch.search.ValueSearch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.MessageFormat;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import static java.util.Comparator.comparing;
import static li.strolch.rest.helper.RestfulHelper.toJson;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE_GET_SESSION;
import static li.strolch.search.SearchBuilder.buildSimpleValueSearch;

@Path("strolch/sessions")
public class UserSessionsResource {

	private static final Logger logger = LoggerFactory.getLogger(UserSessionsResource.class);

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[1];
		return element.getClassName() + "." + element.getMethodName();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response querySessions(@Context HttpServletRequest request, @BeanParam QueryData queryData) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		String source = (String) request.getAttribute(StrolchRestfulConstants.STROLCH_REQUEST_SOURCE);
		logger.info("[{}] Querying user sessions...", cert.getUsername());
		StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_SESSION);

			String query = queryData.getQuery();
			List<UserSession> sessions = sessionHandler.getSessions(cert, source);

			SearchResult<UserSession> result = buildSimpleValueSearch(new ValueSearch<UserSession>(), query,
					Arrays.asList(UserSession::username, UserSession::firstName, UserSession::lastName,
							UserSession::userRoles)).search(sessions).orderBy(comparing(UserSession::username));

			JsonObject root = toJson(queryData, sessions.size(), result, UserSession::toJson);
			Gson gson = new GsonBuilder().setPrettyPrinting().create();
			return Response.ok(gson.toJson(root), MediaType.APPLICATION_JSON).build();
		}
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{sessionId}")
	public Response getSession(@Context HttpServletRequest request, @PathParam("sessionId") String sessionId) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		String source = (String) request.getAttribute(StrolchRestfulConstants.STROLCH_REQUEST_SOURCE);
		logger.info("[{}] Returning session {}", cert.getUsername(), sessionId);
		StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_SESSION);

			UserSession session = sessionHandler.getSession(cert, source, sessionId);
			return Response.ok(session.toJson().toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{sessionId}")
	public Response invalidateSession(@Context HttpServletRequest request, @PathParam("sessionId") String sessionId) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		logger.info("[{}] Invalidating session {}", cert.getUsername(), sessionId);
		StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {
			tx.getPrivilegeContext().assertHasPrivilege(PRIVILEGE_GET_SESSION);

			sessionHandler.invalidate(cert, sessionId);
			return ResponseUtil.toResponse();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{sessionId}/locale/{locale}")
	public Response setSessionLocale(@Context HttpServletRequest request, @PathParam("sessionId") String sessionId,
			@PathParam("locale") String localeS) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		logger.info("[{}] Setting locale of session {} to {}", cert.getUsername(), sessionId, localeS);
		StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
		Locale locale;
		try {
			locale = Locale.forLanguageTag(localeS);
		} catch (Exception e) {
			String msg = MessageFormat.format("Locale {0} is not valid!", localeS);
			return Response.serverError().entity(ResponseUtil.toResponse(msg)).type(MediaType.APPLICATION_JSON).build();
		}

		sessionHandler.setSessionLocale(cert, sessionId, locale);
		return ResponseUtil.toResponse();
	}
}
