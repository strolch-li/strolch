package li.strolch.rest.endpoint;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.soql.core.QueryProcessor;
import li.strolch.soql.core.QueryRequest;
import li.strolch.soql.core.QueryResponse;

@Path("strolch/model")
public class ModelQuery {

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Path("soql")
	public Response doQuery(@Context HttpServletRequest request, @QueryParam("realmName") String realmName,
			@QueryParam("flat") String flat, String data) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();

		QueryResponse queryResponse;
		try (StrolchTransaction tx = openTx(cert, realmName)) {
			QueryRequest queryRequest = QueryRequest.fromJson(jsonObject);
			QueryProcessor queryProcessor = new QueryProcessor();
			queryResponse = queryProcessor.process(queryRequest, tx);
		}

		return Response.ok(queryResponse.asJson(Boolean.parseBoolean(flat)).toString(), MediaType.APPLICATION_JSON)
				.build();
	}

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().getContainer().getRealm(realm)
				.openTx(certificate, ModelQuery.class);
	}
}
