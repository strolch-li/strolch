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

import com.google.gson.JsonArray;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.audit.Audit;
import li.strolch.model.json.AuditToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.AuditQueryData;
import li.strolch.rest.model.visitor.ToAuditQueryVisitor;

@Path("strolch/audits")
public class AuditsService {

	@GET
	@Path("types")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response queryTypes(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		StrolchRealm realm = RestfulStrolchComponent.getInstance().getContainer().getRealm(cert);

		try (StrolchTransaction tx = realm.openTx(cert, AuditsService.class)) {
			JsonArray dataJ = new JsonArray();
			tx.getAuditTrail().getTypes(tx).forEach(dataJ::add);
			return Response.ok(dataJ.toString(), MediaType.APPLICATION_JSON).build();
		}
	}

	@GET
	@Path("query")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response queryAudits(@BeanParam AuditQueryData query, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		StrolchRealm realm = RestfulStrolchComponent.getInstance().getContainer().getRealm(cert);

		try (StrolchTransaction tx = realm.openTx(cert, AuditsService.class)) {

			li.strolch.model.audit.AuditQuery<Audit> auditQuery = new ToAuditQueryVisitor().create(query);
			JsonArray dataJ = new JsonArray();
			tx.getAuditTrail().doQuery(tx, auditQuery).forEach(a -> a.accept(new AuditToJsonVisitor()));
			return Response.ok(dataJ.toString(), MediaType.APPLICATION_JSON).build();
		}
	}
}
