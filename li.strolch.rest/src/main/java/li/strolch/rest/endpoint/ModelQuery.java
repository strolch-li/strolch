package li.strolch.rest.endpoint;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.json.StrolchElementToJsonVisitor;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.model.query.parser.QueryParser;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.RestfulHelper;
import li.strolch.rest.model.QueryData;

@Path("strolch/model")
public class ModelQuery {

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Path("query")
	public Response doQuery(@Context HttpServletRequest request, @QueryParam("realmName") String realmName) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		List<JsonObject> result = new ArrayList<>();
		try (StrolchTransaction tx = openTx(cert, realmName)) {

			StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();
			visitor.flat();

			// TODO MS do query

		}

		JsonObject json = new JsonObject();
		JsonArray arrJ = new JsonArray();
		result.forEach(arrJ::add);

		return Response.ok(json, MediaType.APPLICATION_JSON).build();
	}

	/**
	 * Query {@link Resource Resources} by parsing the query string in {@link QueryData#getQuery()} using
	 * {@link QueryParser}
	 *
	 * @param queryData the data from the client
	 * @param request   the {@link HttpServletRequest} on which to get the {@link Certificate}
	 * @return {@link Response} containing the JSONified {@link Resource Resources} queried
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("resources")
	public Response queryResources(@BeanParam QueryData queryData, @Context HttpServletRequest request) {

		queryData.initializeUnsetFields();
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		List<Resource> resources = new ArrayList<>();

		// parse the query string
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery(queryData.getQuery(), true, true, true);

		// query the data
		long dataSetSize = 0L;
		try (StrolchTransaction tx = openTx(cert, queryData.getRealmName())) {
			ResourceMap resourceMap = tx.getResourceMap();

			if (query.hasNavigation()) {
				String type = ((StrolchTypeNavigation) query.getNavigation()).getType();
				dataSetSize = resourceMap.querySize(tx, type);
				resources.addAll(tx.doQuery(query));
			} else {
				Set<String> types = resourceMap.getTypes(tx);
				dataSetSize = resourceMap.querySize(tx);
				for (String type : types) {
					query.setNavigation(new StrolchTypeNavigation(type));
					resources.addAll(tx.doQuery(query));
				}
			}
		}

		// do ordering
		RestfulHelper.doOrdering(queryData, resources);

		// build JSON response
		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();
		JsonObject root = RestfulHelper.toJson(queryData, dataSetSize, resources, visitor);

		// marshall result
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		String entity = gson.toJson(root);
		return Response.ok(entity).build();
	}

	/**
	 * Query {@link Order Orders} by parsing the query string in {@link QueryData#getQuery()} using {@link QueryParser}
	 *
	 * @param queryData the data from the client
	 * @param request   the {@link HttpServletRequest} on which to get the {@link Certificate}
	 * @return {@link Response} containing the JSONified {@link Order Orders} queried
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("orders")
	public Response queryOrders(@BeanParam QueryData queryData, @Context HttpServletRequest request) {

		queryData.initializeUnsetFields();
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		List<Order> orders = new ArrayList<>();

		// parse the query string
		OrderQuery<Order> query = QueryParser.parseToOrderQuery(queryData.getQuery(), true, true, true);

		// query the data
		long dataSetSize = 0L;
		try (StrolchTransaction tx = openTx(cert, queryData.getRealmName())) {
			OrderMap orderMap = tx.getOrderMap();

			if (query.hasNavigation()) {
				String type = ((StrolchTypeNavigation) query.getNavigation()).getType();
				dataSetSize = orderMap.querySize(tx, type);
				orders.addAll(tx.doQuery(query));
			} else {
				Set<String> types = orderMap.getTypes(tx);
				dataSetSize = orderMap.querySize(tx);
				for (String type : types) {
					query.setNavigation(new StrolchTypeNavigation(type));
					orders.addAll(tx.doQuery(query));
				}
			}
		}

		// do ordering
		RestfulHelper.doOrdering(queryData, orders);

		// build JSON response
		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();
		JsonObject root = RestfulHelper.toJson(queryData, dataSetSize, orders, visitor);

		// marshall result
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		String entity = gson.toJson(root);
		return Response.ok(entity).build();
	}

	/**
	 * Query {@link Activity Activities} by parsing the query string in {@link QueryData#getQuery()} using
	 * {@link QueryParser}
	 *
	 * @param queryData the data from the client
	 * @param request   the {@link HttpServletRequest} on which to get the {@link Certificate}
	 * @return {@link Response} containing the JSONified {@link Activity Activities} queried
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("activities")
	public Response queryActivities(@BeanParam QueryData queryData, @Context HttpServletRequest request) {

		queryData.initializeUnsetFields();
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		List<Activity> activities = new ArrayList<>();

		// parse the query string
		ActivityQuery<Activity> query = QueryParser.parseToActivityQuery(queryData.getQuery(), true, true, true);

		// query the data
		long dataSetSize = 0L;
		try (StrolchTransaction tx = openTx(cert, queryData.getRealmName())) {
			ActivityMap activityMap = tx.getActivityMap();

			if (query.hasNavigation()) {
				String type = ((StrolchTypeNavigation) query.getNavigation()).getType();
				dataSetSize = activityMap.querySize(tx, type);
				activities.addAll(tx.doQuery(query));
			} else {
				Set<String> types = activityMap.getTypes(tx);
				dataSetSize = activityMap.querySize(tx);
				for (String type : types) {
					query.setNavigation(new StrolchTypeNavigation(type));
					activities.addAll(tx.doQuery(query));
				}
			}
		}

		// do ordering
		RestfulHelper.doOrdering(queryData, activities);

		// build JSON response
		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();
		JsonObject root = RestfulHelper.toJson(queryData, dataSetSize, activities, visitor);

		// marshall result
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		String entity = gson.toJson(root);
		return Response.ok(entity).build();
	}

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().getContainer().getRealm(realm)
				.openTx(certificate, ModelQuery.class);
	}
}
