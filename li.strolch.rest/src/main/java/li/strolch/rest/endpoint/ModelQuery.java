package li.strolch.rest.endpoint;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;

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

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[2];
		return element.getClassName() + "." + element.getMethodName();
	}

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().openTx(certificate, realm, getContext());
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Path("soql")
	public Response doQuery(@Context HttpServletRequest request, @QueryParam("realmName") String realmName,
			@QueryParam("flat") String flat, String data) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();

		QueryResponse queryResponse;
		try (StrolchTransaction tx = openTx(cert, realmName)) {
			QueryRequest queryRequest = QueryRequest.fromJson(jsonObject);
			QueryProcessor queryProcessor = new QueryProcessor();
			queryResponse = queryProcessor.process(queryRequest, tx);
		}

		return Response.ok(queryResponse.asJson(Boolean.parseBoolean(flat)).toString(), MediaType.APPLICATION_JSON)
				.build();
	}
}
