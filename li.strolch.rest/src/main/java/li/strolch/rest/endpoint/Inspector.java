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

import java.io.StringReader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BeanParam;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.model.json.ActivityFromJsonVisitor;
import li.strolch.model.json.FromFlatJsonVisitor;
import li.strolch.model.json.OrderFromJsonVisitor;
import li.strolch.model.json.ResourceFromJsonVisitor;
import li.strolch.model.json.StrolchElementToJsonVisitor;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.model.query.parser.QueryParser;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.StrolchElementToXmlStringVisitor;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.RestfulHelper;
import li.strolch.rest.model.Result;
import li.strolch.service.LocatorArgument;
import li.strolch.service.RemoveActivityService;
import li.strolch.service.RemoveOrderService;
import li.strolch.service.RemoveResourceService;
import li.strolch.service.UpdateActivityService;
import li.strolch.service.UpdateActivityService.UpdateActivityArg;
import li.strolch.service.UpdateOrderService;
import li.strolch.service.UpdateOrderService.UpdateOrderArg;
import li.strolch.service.UpdateResourceService;
import li.strolch.service.UpdateResourceService.UpdateResourceArg;
import li.strolch.service.api.ServiceResult;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/inspector")
public class Inspector {

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().getContainer().getRealm(realm).openTx(certificate,
				Inspector.class);
	}

	private String toString(JsonObject jsonObject) {
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		return gson.toJson(jsonObject);
	}

	/**
	 * <p>
	 * Root path of the inspector
	 * </p>
	 * 
	 * <p>
	 * Returns the root element, which is an overview of the configured realms
	 * </p>
	 * 
	 * @return the root element, which is an overview of the configured realms
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAgent(@Context HttpServletRequest request) {
		try {

			Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

			JsonObject agentOverview = new JsonObject();
			JsonArray realmsArr = new JsonArray();
			agentOverview.add(Tags.Json.REALMS, realmsArr);

			ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
			Set<String> realmNames = container.getRealmNames();
			for (String realmName : realmNames) {

				JsonObject realmJ = new JsonObject();

				try (StrolchTransaction tx = openTx(cert, realmName)) {
					long size = 0;
					size += tx.getResourceMap().querySize(tx);
					size += tx.getOrderMap().querySize(tx);

					realmJ.addProperty(Tags.Json.NAME, realmName);
					realmJ.addProperty(Tags.Json.SIZE, size);

					realmsArr.add(realmJ);
				}
			}

			return Response.ok().entity(toString(agentOverview)).build();

		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * <p>
	 * Realm inspector
	 * </p>
	 * 
	 * <p>
	 * Returns the overview of a specific relam
	 * </p>
	 * 
	 * @param realm
	 *            the realm for which the overview is to be returned
	 * 
	 * @return the overview of a specific relam
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}")
	public Response getRealm(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		JsonObject realmDetailJ = new JsonObject();
		JsonArray elementMapsArr = new JsonArray();
		realmDetailJ.add(Tags.Json.ELEMENT_MAPS, elementMapsArr);

		try (StrolchTransaction tx = openTx(cert, realm)) {

			{
				ResourceMap resourceMap = tx.getResourceMap();
				JsonObject elementMapJ = new JsonObject();
				elementMapJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.RESOURCE);
				elementMapJ.addProperty(Tags.Json.NR_OF_ELEMENTS, resourceMap.querySize(tx));
				JsonArray typesJ = new JsonArray();
				resourceMap.getTypes(tx).forEach(type -> typesJ.add(new JsonPrimitive(type)));
				elementMapJ.add(Tags.Json.TYPES, typesJ);

				elementMapsArr.add(elementMapJ);
			}

			{
				OrderMap orderMap = tx.getOrderMap();
				JsonObject elementMapJ = new JsonObject();
				elementMapJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ORDER);
				elementMapJ.addProperty(Tags.Json.NR_OF_ELEMENTS, orderMap.querySize(tx));
				JsonArray typesJ = new JsonArray();
				orderMap.getTypes(tx).forEach(type -> typesJ.add(new JsonPrimitive(type)));
				elementMapJ.add(Tags.Json.TYPES, typesJ);

				elementMapsArr.add(elementMapJ);
			}

			{
				ActivityMap activityMap = tx.getActivityMap();
				JsonObject elementMapJ = new JsonObject();
				elementMapJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ACTIVITY);
				elementMapJ.addProperty(Tags.Json.NR_OF_ELEMENTS, activityMap.querySize(tx));
				JsonArray typesJ = new JsonArray();
				activityMap.getTypes(tx).forEach(type -> typesJ.add(new JsonPrimitive(type)));
				elementMapJ.add(Tags.Json.TYPES, typesJ);

				elementMapsArr.add(elementMapJ);
			}
		}

		return Response.ok().entity(toString(realmDetailJ)).build();
	}

	/**
	 * <p>
	 * Resource inspector
	 * </p>
	 * <p>
	 * Returns an overview of the {@link Resource Resources}. This is a list of all the types and the size each type has
	 * </p>
	 * 
	 * @param realm
	 *            the realm for which the resource overview is to be returned
	 * 
	 * @return an overview of the {@link Resource Resources}. This is a list of all the types and the size each type has
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources")
	public Response getResourcesOverview(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		JsonObject mapOverview = new JsonObject();

		try (StrolchTransaction tx = openTx(cert, realm)) {
			ResourceMap resourceMap = tx.getResourceMap();

			mapOverview.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.RESOURCE);
			mapOverview.addProperty(Tags.Json.SIZE, resourceMap.querySize(tx));

			JsonArray typeArrJ = new JsonArray();
			mapOverview.add(Tags.Json.TYPES, typeArrJ);

			List<String> types = new ArrayList<>(resourceMap.getTypes(tx));
			Collections.sort(types);
			types.forEach(type -> {

				JsonObject typeJ = new JsonObject();
				typeJ.addProperty(Tags.Json.TYPE, type);
				typeJ.addProperty(Tags.Json.SIZE, resourceMap.querySize(tx, type));

				typeArrJ.add(typeJ);
			});
		}

		return Response.ok().entity(toString(mapOverview)).build();
	}

	/**
	 * <p>
	 * Order inspector
	 * </p>
	 * <p>
	 * Returns an overview of the {@link Order Orders}. This is a list of all the types and the size each type has
	 * </p>
	 * 
	 * @param realm
	 *            the realm for which the order overview is to be returned
	 * 
	 * @return an overview of the {@link Order Orders}. This is a list of all the types and the size each type has
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders")
	public Response getOrdersOverview(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		JsonObject mapOverview = new JsonObject();

		try (StrolchTransaction tx = openTx(cert, realm)) {
			OrderMap orderMap = tx.getOrderMap();

			mapOverview.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ORDER);
			mapOverview.addProperty(Tags.Json.SIZE, orderMap.querySize(tx));

			JsonArray typeArrJ = new JsonArray();
			mapOverview.add(Tags.Json.TYPES, typeArrJ);

			List<String> types = new ArrayList<>(orderMap.getTypes(tx));
			Collections.sort(types);
			types.forEach(type -> {

				JsonObject typeJ = new JsonObject();
				typeJ.addProperty(Tags.Json.TYPE, type);
				typeJ.addProperty(Tags.Json.SIZE, orderMap.querySize(tx, type));

				typeArrJ.add(typeJ);
			});
		}

		return Response.ok().entity(toString(mapOverview)).build();
	}

	/**
	 * <p>
	 * Activity inspector
	 * </p>
	 * <p>
	 * Returns an overview of the {@link Activity Activities}. This is a list of all the types and the size each type
	 * has
	 * </p>
	 * 
	 * @param realm
	 *            the realm for which the activity overview is to be returned
	 * 
	 * @return an overview of the {@link Activity Activities}. This is a list of all the types and the size each type
	 *         has
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities")
	public Response getActivitiesOverview(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		JsonObject mapOverview = new JsonObject();

		try (StrolchTransaction tx = openTx(cert, realm)) {
			ActivityMap activityMap = tx.getActivityMap();

			mapOverview.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ACTIVITY);
			mapOverview.addProperty(Tags.Json.SIZE, activityMap.querySize(tx));

			JsonArray typeArrJ = new JsonArray();
			mapOverview.add(Tags.Json.TYPES, typeArrJ);

			List<String> types = new ArrayList<>(activityMap.getTypes(tx));
			Collections.sort(types);
			types.forEach(type -> {

				JsonObject typeJ = new JsonObject();
				typeJ.addProperty(Tags.Json.TYPE, type);
				typeJ.addProperty(Tags.Json.SIZE, activityMap.querySize(tx, type));

				typeArrJ.add(typeJ);
			});
		}

		return Response.ok().entity(toString(mapOverview)).build();
	}

	// TODO for the get element type details, we should not simply query all objects, but rather find a solution to query only the id, name, type and date, state for the order

	/**
	 * <p>
	 * Resource type inspector
	 * </p>
	 * <p>
	 * Returns an overview of the {@link Resource Resources} with the given type. This is a list of overviews of the
	 * resources
	 * </p>
	 * 
	 * @param realm
	 *            the realm for which the resource type overview is to be returned
	 * @param type
	 *            marshall
	 * @return an overview of the {@link Resource Resources} with the given type. This is a list of overviews of the
	 *         resources
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}")
	public Response queryResourcesByType(@Context HttpServletRequest request, @BeanParam QueryData queryData,
			@PathParam("realm") String realm, @PathParam("type") String type) {

		queryData.initializeUnsetFields();
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		List<Resource> resources = new ArrayList<>();

		// parse the query string
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery(queryData.getQuery(), true, true, false);

		// set navigation to requested type
		query.setNavigation(new StrolchTypeNavigation(type));

		// query the data
		long dataSetSize = 0L;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			ResourceMap resourceMap = tx.getResourceMap();
			dataSetSize = resourceMap.querySize(tx);
			resources.addAll(tx.doQuery(query));
		}

		// do ordering
		RestfulHelper.doOrdering(queryData, resources);

		// build JSON response
		StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor();
		JsonObject root = RestfulHelper.toJson(queryData, dataSetSize, resources, toJsonVisitor);

		// marshall result
		return Response.ok(toString(root)).build();
	}

	/**
	 * <p>
	 * Order type inspector
	 * </p>
	 * <p>
	 * Returns an overview of the {@link Order Orders} with the given type. This is a list of overviews of the orders
	 * </p>
	 * 
	 * @param realm
	 *            the realm for which the order type overview is to be returned
	 * @param type
	 * 
	 * @return an overview of the {@link Order Orders} with the given type. This is a list of overviews of the orders
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}")
	public Response queryOrdersByType(@Context HttpServletRequest request, @BeanParam QueryData queryData,
			@PathParam("realm") String realm, @PathParam("type") String type) {

		queryData.initializeUnsetFields();
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		List<Order> orders = new ArrayList<>();

		// parse the query string
		OrderQuery<Order> query = QueryParser.parseToOrderQuery(queryData.getQuery(), true, true, false);
		query.setNavigation(new StrolchTypeNavigation(type));

		// query the data
		long dataSetSize = 0L;
		try (StrolchTransaction tx = openTx(cert, queryData.getRealmName())) {
			OrderMap orderMap = tx.getOrderMap();
			dataSetSize = orderMap.querySize(tx);
			orders.addAll(tx.doQuery(query));
		}

		// do ordering
		RestfulHelper.doOrdering(queryData, orders);

		// build JSON response
		StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor();
		JsonObject root = RestfulHelper.toJson(queryData, dataSetSize, orders, toJsonVisitor);

		// marshall result
		return Response.ok(toString(root)).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}")
	public Response queryActivitiesByType(@BeanParam QueryData queryData, @PathParam("realm") String realm,
			@PathParam("type") String type, @Context HttpServletRequest request) {

		queryData.initializeUnsetFields();
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		List<Activity> activities = new ArrayList<>();

		// parse the query string
		ActivityQuery<Activity> query = QueryParser.parseToActivityQuery(queryData.getQuery(), true, true, false);
		query.setNavigation(new StrolchTypeNavigation(type));

		// query the data
		long dataSetSize = 0L;
		try (StrolchTransaction tx = openTx(cert, queryData.getRealmName())) {
			ActivityMap activityMap = tx.getActivityMap();
			dataSetSize = activityMap.querySize(tx);
			activities.addAll(tx.doQuery(query));
		}

		// do ordering
		RestfulHelper.doOrdering(queryData, activities);

		// build JSON response
		StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor();
		JsonObject root = RestfulHelper.toJson(queryData, dataSetSize, activities, toJsonVisitor);

		// marshall result
		return Response.ok(toString(root)).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}/{id}")
	public Response getResourceAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") String flat) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Resource resource;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			resource = tx.getResourceMap().getBy(tx, type, id);
		}
		if (resource == null) {
			throw new StrolchException(MessageFormat.format("No Resource exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();
		if (Boolean.parseBoolean(flat))
			visitor.flat();
		return Response.ok().entity(toString(resource.accept(visitor))).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/resources/{type}/{id}")
	public Response getResourceAsXml(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Resource resource;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			resource = tx.getResourceMap().getBy(tx, type, id);
		}
		if (resource == null) {
			throw new StrolchException(MessageFormat.format("No Resource exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		String asXml = resource.accept(new StrolchElementToXmlStringVisitor());
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/resources/{type}/{id}")
	public Response updateResourceAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Resource resource;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getResources().size() == 0)
				throw new StrolchPersistenceException(
						MessageFormat.format("No Resources parsed from xml value for {0} / {1}", id, type));
			if (listener.getResources().size() > 1)
				throw new StrolchPersistenceException(
						MessageFormat.format("Multiple Resources parsed from xml value for {0} / {1}", id, type));

			resource = listener.getResources().get(0);

		} catch (Exception e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to extract Resources from xml value for {0} / {1}", id, type), e);
		}

		UpdateResourceService svc = new UpdateResourceService();
		UpdateResourceArg arg = new UpdateResourceArg();
		arg.resource = resource;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = resource.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return Result.toResponse(result);
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}/{id}")
	public Response updateResourceAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") String flatS, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		boolean flat = Boolean.parseBoolean(flatS);

		UpdateResourceService svc = new UpdateResourceService();
		UpdateResourceArg arg = new UpdateResourceArg();

		// parse JSON string
		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();
		Resource resource;
		if (flat) {

			// parse from flat JSON
			try (StrolchTransaction tx = openTx(cert, realm)) {
				resource = tx.getResourceBy(type, id, true);
			}
			new FromFlatJsonVisitor().visit(resource, jsonObject);

		} else {

			// parse from complete JSON
			ResourceFromJsonVisitor visitor = new ResourceFromJsonVisitor();
			resource = visitor.visit(jsonObject);

			// we are missing a version, so:
			arg.refreshUnknownVersion = true;
		}

		// prepare argument
		arg.resource = resource;
		arg.realm = realm;

		// do service
		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor();
			if (flat)
				toJsonVisitor.flat();
			return Response.ok().entity(toString(resource.accept(toJsonVisitor))).build();
		}

		return Result.toResponse(result);
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}/{id}")
	public Response getOrderAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") String flat) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Order order;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			order = tx.getOrderMap().getBy(tx, type, id);
		}
		if (order == null) {
			throw new StrolchException(MessageFormat.format("No Order exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();
		if (Boolean.parseBoolean(flat))
			visitor.flat();
		return Response.ok().entity(toString(order.accept(visitor))).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/{type}/{id}")
	public Response getOrderAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Order order;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			order = tx.getOrderMap().getBy(tx, type, id);
		}
		if (order == null) {
			throw new StrolchException(MessageFormat.format("No Order exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		String asXml = order.accept(new StrolchElementToXmlStringVisitor());
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/{type}/{id}")
	public Response updateOrderAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Order order;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getOrders().size() == 0)
				throw new StrolchPersistenceException(
						MessageFormat.format("No Orders parsed from xml value for {0} / {1}", id, type));
			if (listener.getOrders().size() > 1)
				throw new StrolchPersistenceException(
						MessageFormat.format("Multiple Orders parsed from xml value for {0} / {1}", id, type));

			order = listener.getOrders().get(0);

		} catch (Exception e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to extract Order from xml value for {0} / {1}", id, type), e);
		}

		UpdateOrderService svc = new UpdateOrderService();
		UpdateOrderArg arg = new UpdateOrderArg();
		arg.order = order;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = order.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return Result.toResponse(result);
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}/{id}")
	public Response updateOrderAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") String flatS, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		boolean flat = Boolean.parseBoolean(flatS);

		UpdateOrderService svc = new UpdateOrderService();
		UpdateOrderArg arg = new UpdateOrderArg();

		// parse JSON string
		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();
		Order order;
		if (flat) {

			// parse from flat JSON
			try (StrolchTransaction tx = openTx(cert, realm)) {
				order = tx.getOrderBy(type, id, true);
			}
			new FromFlatJsonVisitor().visit(order, jsonObject);

		} else {

			// parse from complete JSON
			OrderFromJsonVisitor visitor = new OrderFromJsonVisitor();
			order = visitor.visit(jsonObject);

			// we are missing a version, so:
			arg.refreshUnknownVersion = true;
		}

		// prepare argument
		arg.order = order;
		arg.realm = realm;

		// do service
		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor();
			if (flat)
				toJsonVisitor.flat();
			return Response.ok().entity(toString(order.accept(toJsonVisitor))).build();
		}

		return Result.toResponse(result);
	}

	/**
	 * <p>
	 * Activity inspector
	 * </p>
	 * 
	 * <p>
	 * Returns the activity with the given id
	 * </p>
	 * 
	 * @param realm
	 *            the realm for which the activity is to be returned
	 * @param type
	 *            the type of the activity
	 * @param id
	 *            the id of the activity
	 * 
	 * @return the activity with the given id
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}/{id}")
	public Response getActivityAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") String flat) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Activity activity;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			activity = tx.getActivityMap().getBy(tx, type, id);
		}
		if (activity == null) {
			throw new StrolchException(MessageFormat.format("No Activity exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();
		if (Boolean.parseBoolean(flat))
			visitor.flat();
		return Response.ok().entity(toString(activity.accept(visitor))).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/{type}/{id}")
	public Response getActivityAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Activity activity;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			activity = tx.getActivityMap().getBy(tx, type, id);
		}
		if (activity == null) {
			throw new StrolchException(MessageFormat.format("No Activity exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		String asXml = activity.accept(new StrolchElementToXmlStringVisitor());
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/{type}/{id}")
	public Response updateActivityAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Activity activity;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getActivities().size() == 0)
				throw new StrolchPersistenceException(
						MessageFormat.format("No Activities parsed from xml value for {0} / {1}", id, type));
			if (listener.getActivities().size() > 1)
				throw new StrolchPersistenceException(
						MessageFormat.format("Multiple Activities parsed from xml value for {0} / {1}", id, type));

			activity = listener.getActivities().get(0);

		} catch (Exception e) {
			throw new StrolchPersistenceException(
					MessageFormat.format("Failed to extract Activities from xml value for {0} / {1}", id, type), e);
		}

		UpdateActivityService svc = new UpdateActivityService();
		UpdateActivityArg arg = new UpdateActivityArg();
		arg.activity = activity;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = activity.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return Result.toResponse(result);
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}/{id}")
	public Response updateActivityAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") String flatS, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		boolean flat = Boolean.parseBoolean(flatS);

		UpdateActivityService svc = new UpdateActivityService();
		UpdateActivityArg arg = new UpdateActivityArg();

		// parse JSON string
		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();
		Activity activity;
		if (flat) {

			// parse from flat JSON
			try (StrolchTransaction tx = openTx(cert, realm)) {
				activity = tx.getActivityBy(type, id, true);
			}
			new FromFlatJsonVisitor().visit(activity, jsonObject);

		} else {

			// parse from complete JSON
			ActivityFromJsonVisitor visitor = new ActivityFromJsonVisitor();
			activity = visitor.visit(jsonObject);

			// we are missing a version, so:
			arg.refreshUnknownVersion = true;
		}

		// prepare argument
		arg.activity = activity;
		arg.realm = realm;

		// do service
		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor();
			if (flat)
				toJsonVisitor.flat();
			return Response.ok().entity(toString(activity.accept(toJsonVisitor))).build();
		}

		return Result.toResponse(result);
	}

	@DELETE
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/resources/{type}/{id}")
	public Response removeResource(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		RemoveResourceService svc = new RemoveResourceService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.locator = Resource.locatorFor(type, id);
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		return Result.toResponse(result);
	}

	@DELETE
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/{type}/{id}")
	public Response removeOrder(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		RemoveOrderService svc = new RemoveOrderService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.locator = Order.locatorFor(type, id);
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		return Result.toResponse(result);
	}

	@DELETE
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/{type}/{id}")
	public Response removeActivity(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		RemoveActivityService svc = new RemoveActivityService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.locator = Activity.locatorFor(type, id);
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		return Result.toResponse(result);
	}
}
