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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.AuditQuery;
import li.strolch.rest.model.AuditQueryResult;
import li.strolch.rest.model.StringListResult;
import li.strolch.rest.model.visitor.ToAuditQueryVisitor;
import ch.eitchnet.privilege.model.Certificate;

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
			Set<String> data = tx.getAuditTrail().getTypes(tx);
			return Response.ok(new StringListResult(new ArrayList<>(data)), MediaType.APPLICATION_JSON).build();
		}
	}

	@POST
	@Path("query")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response queryAudits(AuditQuery query, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		StrolchRealm realm = RestfulStrolchComponent.getInstance().getContainer().getRealm(cert);

		try (StrolchTransaction tx = realm.openTx(cert, AuditsService.class)) {

			li.strolch.model.audit.AuditQuery<Audit> auditQuery = new ToAuditQueryVisitor().create(query);
			List<Audit> audits = tx.getAuditTrail().doQuery(tx, auditQuery);

			return Response.ok(new AuditQueryResult(audits), MediaType.APPLICATION_JSON).build();
		}
	}
}
