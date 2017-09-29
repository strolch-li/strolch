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

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.text.MessageFormat;
import java.util.Locale;

import com.google.gson.JsonArray;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.StrolchSessionHandler;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.model.UserSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Path("strolch/sessions")
public class UserSessionsService {

	private static final Logger logger = LoggerFactory.getLogger(UserSessionsService.class);

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getSessions(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		logger.info("[" + cert.getUsername() + "] Returning all sessions...");
		StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();

		JsonArray sessionsJ = new JsonArray();
		sessionHandler.getSessions(cert).stream().map(UserSession::toJson).forEach(sessionsJ::add);
		return Response.ok(sessionsJ.toString(), MediaType.APPLICATION_JSON).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{sessionId}")
	public Response getSession(@Context HttpServletRequest request, @PathParam("sessionId") String sessionId) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		logger.info("[" + cert.getUsername() + "] Returning session " + sessionId);
		StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
		UserSession session = sessionHandler.getSession(cert, sessionId);
		return Response.ok(session.toJson().toString(), MediaType.APPLICATION_JSON).build();
	}

	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{sessionId}")
	public Response invalidateSession(@Context HttpServletRequest request, @PathParam("sessionId") String sessionId) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		logger.info("[" + cert.getUsername() + "] Invalidating session " + sessionId);
		StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
		sessionHandler.invalidateSession(cert, sessionId);
		return ResponseUtil.toResponse();
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{sessionId}/locale/{locale}")
	public Response setSessionLocale(@Context HttpServletRequest request, @PathParam("sessionId") String sessionId,
			@PathParam("locale") String localeS) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		logger.info("[" + cert.getUsername() + "] Setting locale of session " + sessionId + " to " + localeS);
		StrolchSessionHandler sessionHandler = RestfulStrolchComponent.getInstance().getSessionHandler();
		Locale locale;
		try {
			locale = new Locale(localeS);
		} catch (Exception e) {
			String msg = MessageFormat.format("Locale {0} is not valid!", localeS);
			return Response.serverError().entity(ResponseUtil.toResponse(msg)).type(MediaType.APPLICATION_JSON).build();
		}

		sessionHandler.setSessionLocale(cert, sessionId, locale);
		return ResponseUtil.toResponse();
	}
}
