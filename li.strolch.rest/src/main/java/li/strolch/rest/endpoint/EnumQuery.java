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
import java.util.Locale;

import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.runtime.query.enums.EnumHandler;
import li.strolch.runtime.query.enums.StrolchEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/enums")
public class EnumQuery {

	private static final Logger logger = LoggerFactory.getLogger(EnumQuery.class);

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{name}")
	public Response getEnum(@Context HttpServletRequest request, @PathParam("name") String name,
			@QueryParam("withoutHidden") boolean withoutHidden) {

		try {

			EnumHandler enumHandler = RestfulStrolchComponent.getInstance()
					.getContainer()
					.getComponent(EnumHandler.class);

			Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
			StrolchEnum strolchEnum = enumHandler.getEnum(cert, name, cert.getLocale(), withoutHidden);
			return Response.ok().entity(strolchEnum.toJson().toString()).build();

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return Response.serverError().entity(e.getMessage()).build();
		}
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{name}/{locale}")
	public Response getEnumBySpecificLocale(@PathParam("name") String name, @PathParam("locale") String locale,
			@Context HttpServletRequest request) {

		try {

			EnumHandler enumHandler = RestfulStrolchComponent.getInstance()
					.getContainer()
					.getComponent(EnumHandler.class);

			Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
			StrolchEnum strolchEnum = enumHandler.getEnum(cert, name, Locale.forLanguageTag(locale));
			return Response.ok().entity(strolchEnum.toJson().toString()).build();

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return Response.serverError().entity(e.getMessage()).build();
		}
	}
}
