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
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import li.strolch.agent.api.VersionQueryResult;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/version")
public class VersionQuery {

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getVersions(@Context HttpServletRequest request) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		RestfulStrolchComponent instance = RestfulStrolchComponent.getInstance();
		instance.getSessionHandler().validate(cert);

		VersionQueryResult versionQueryResult = instance.getContainer().getAgent().getVersion();
		GenericEntity<VersionQueryResult> entity = new GenericEntity<VersionQueryResult>(versionQueryResult,
				VersionQueryResult.class) {
			//
		};
		return Response.ok().entity(entity).build();
	}
}
