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

			li.strolch.model.audit.AuditQuery auditQuery = new ToAuditQueryVisitor().create(query);
			List<Audit> audits = tx.getAuditTrail().doQuery(tx, auditQuery);

			return Response.ok(new AuditQueryResult(audits), MediaType.APPLICATION_JSON).build();
		}
	}
}
