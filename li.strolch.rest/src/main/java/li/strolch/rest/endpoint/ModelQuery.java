package li.strolch.rest.endpoint;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BeanParam;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.StringMatchMode;
import ch.eitchnet.utils.collections.Paging;
import ch.eitchnet.utils.helper.StringHelper;
import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Resource;
import li.strolch.model.json.ResourceToJsonVisitor;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.ParameterSelection;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.StrolchElementOverview;
import li.strolch.rest.model.TypeDetail;
import li.strolch.runtime.StrolchConstants;

@Path("strolch/model")
public class ModelQuery {

	/**
	 * <p>
	 * Query Resources
	 * </p>
	 * 
	 * @param realm
	 *            the realm for which the resource type overview is to be returned
	 * @param type
	 * 
	 * @return an overview of the {@link Resource Resources} with the given type. This is a list of overviews of the
	 *         resources
	 * 
	 * @see TypeDetail
	 * @see StrolchElementOverview
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("resources")
	public Response queryResources(@BeanParam QueryData queryData, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		String realmName = queryData.getRealmName();
		if (StringHelper.isEmpty(realmName))
			realmName = StrolchConstants.DEFAULT_REALM;

		List<Resource> resources = new ArrayList<>();

		Set<String> queryBy = queryData.getQueryByNames();
		Set<String> queryTypes = queryData.getTypes();

		long dataSetSize = 0L;
		try (StrolchTransaction tx = openTx(cert, realmName)) {
			ResourceMap resourceMap = tx.getResourceMap();

			if (queryTypes.isEmpty()) {
				Set<String> types = resourceMap.getTypes(tx);
				dataSetSize = resourceMap.querySize(tx);
				for (String type : types) {
					query(queryData, resources, queryBy, tx, type);
				}
			} else {
				for (String type : queryTypes) {
					dataSetSize = resourceMap.querySize(tx, type);
					query(queryData, resources, queryBy, tx, type);
				}
			}

			tx.doNothingOnClose();
		}

		String orderBy = queryData.getOrderBy();
		boolean asc = queryData.isAscending();
		if (StringHelper.isNotEmpty(orderBy)) {
			if (orderBy.equals(QueryData.ID)) {
				resources.sort((r1, r2) -> asc ? r1.getId().compareTo(r2.getId()) : r2.getId().compareTo(r1.getId()));
			} else if (orderBy.equals(QueryData.NAME)) {
				resources.sort(
						(r1, r2) -> asc ? r1.getName().compareTo(r2.getName()) : r2.getName().compareTo(r1.getName()));
			} else if (orderBy.equals(QueryData.TYPE)) {
				resources.sort(
						(r1, r2) -> asc ? r1.getType().compareTo(r2.getType()) : r2.getType().compareTo(r1.getType()));
			} else {
				String[] parts = orderBy.split(":");
				String bagKey = parts[0];
				String paramKey = parts[1];
				resources.sort((r1, r2) -> {
					Parameter<?> param1 = r1.getParameter(bagKey, paramKey);
					Parameter<?> param2 = r2.getParameter(bagKey, paramKey);
					if (asc)
						return param1.getValueAsString().compareTo(param2.getValueAsString());
					return param2.getValueAsString().compareTo(param1.getValueAsString());
				});
			}
		}

		// paging
		Paging<Resource> paging = Paging.asPage(resources, queryData.getPageSize(), queryData.getPage());

		// get page
		List<Resource> page = paging.getPage();

		// build JSON response
		JsonObject root = new JsonObject();
		root.addProperty("msg", "-");
		root.addProperty("draw", queryData.getDraw());
		root.addProperty("dataSetSize", dataSetSize);
		root.addProperty("nrOfElements", paging.getNrOfElements());

		if (StringHelper.isNotEmpty(orderBy))
			root.addProperty("sortBy", orderBy);
		root.addProperty("ascending", asc);
		root.addProperty("nrOfPages", paging.getNrOfPages());
		root.addProperty("pageSize", paging.getPageSize());
		root.addProperty("page", paging.getPageToReturn());

		// add items
		JsonArray data = new JsonArray();
		for (Resource resource : page) {
			JsonObject element = new ResourceToJsonVisitor().visit(resource);
			data.add(element);
		}
		root.add("data", data);

		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		String entity = gson.toJson(root);
		return Response.ok(entity).build();
	}

	private void query(QueryData queryData, List<Resource> resources, Set<String> queryBy, StrolchTransaction tx,
			String type) {
		ResourceQuery<Resource> query = ResourceQuery.query(type);

		OrSelection or = new OrSelection();

		for (String queryB : queryBy) {
			if (queryB.equals(QueryData.TYPE))
				continue;

			if (queryB.equals(QueryData.ID)) {
				or.with(new IdSelection(queryData.getQuery(), StringMatchMode.ci()));
			} else if (queryB.equals(QueryData.NAME)) {
				or.with(new NameSelection(queryData.getQuery(), StringMatchMode.ci()));
			} else {
				String[] parts = queryB.split(":");
				String bagKey = parts[0];
				String paramKey = parts[1];
				or.with(ParameterSelection.anyTypeSelection(bagKey, paramKey, queryData.getQuery(),
						StringMatchMode.ci()));
			}
		}

		if (or.hasSelection())
			query.with(or);
		else
			query.withAny();

		resources.addAll(tx.doQuery(query));
	}

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().getContainer().getRealm(realm).openTx(certificate,
				Inspector.class);
	}
}
