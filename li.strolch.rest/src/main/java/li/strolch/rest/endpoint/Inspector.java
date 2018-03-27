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

import static li.strolch.rest.StrolchRestfulConstants.MSG;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.StreamingOutput;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.StringReader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.google.gson.*;
import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.Tags.Json;
import li.strolch.model.activity.Activity;
import li.strolch.model.json.*;
import li.strolch.model.visitor.ActivityVisitor;
import li.strolch.model.visitor.OrderVisitor;
import li.strolch.model.visitor.ResourceVisitor;
import li.strolch.model.xml.*;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.helper.RestfulHelper;
import li.strolch.rest.model.QueryData;
import li.strolch.search.*;
import li.strolch.service.*;
import li.strolch.service.AddActivityService.AddActivityArg;
import li.strolch.service.AddOrderService.AddOrderArg;
import li.strolch.service.AddResourceService.AddResourceArg;
import li.strolch.service.UpdateActivityService.UpdateActivityArg;
import li.strolch.service.UpdateOrderService.UpdateOrderArg;
import li.strolch.service.UpdateResourceService.UpdateResourceArg;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.InputSource;

/**
 * The RESTful inspector for Strolch. It allows to inspect the realms, and their respective elements. Supporting querying and retrieving, in multiple formats: XML, JSON and flat JSON
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/inspector")
public class Inspector {

	private static final Logger logger = LoggerFactory.getLogger(Inspector.class);

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().getContainer().getRealm(realm)
				.openTx(certificate, Inspector.class);
	}

	private String toString(JsonElement jsonElement) {
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		return gson.toJson(jsonElement);
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAgentOverview(@Context HttpServletRequest request) {
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
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}")
	public Response getRealmOverview(@Context HttpServletRequest request, @PathParam("realm") String realm) {

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
				resourceMap.getTypes(tx).forEach(typesJ::add);
				elementMapJ.add(Tags.Json.TYPES, typesJ);

				elementMapsArr.add(elementMapJ);
			}

			{
				OrderMap orderMap = tx.getOrderMap();
				JsonObject elementMapJ = new JsonObject();
				elementMapJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ORDER);
				elementMapJ.addProperty(Tags.Json.NR_OF_ELEMENTS, orderMap.querySize(tx));
				JsonArray typesJ = new JsonArray();
				orderMap.getTypes(tx).forEach(typesJ::add);
				elementMapJ.add(Tags.Json.TYPES, typesJ);

				elementMapsArr.add(elementMapJ);
			}

			{
				ActivityMap activityMap = tx.getActivityMap();
				JsonObject elementMapJ = new JsonObject();
				elementMapJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ACTIVITY);
				elementMapJ.addProperty(Tags.Json.NR_OF_ELEMENTS, activityMap.querySize(tx));
				JsonArray typesJ = new JsonArray();
				activityMap.getTypes(tx).forEach(typesJ::add);
				elementMapJ.add(Tags.Json.TYPES, typesJ);

				elementMapsArr.add(elementMapJ);
			}
		}

		return Response.ok().entity(toString(realmDetailJ)).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/xml")
	public Response exportRealmToXml(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm)) {
				XMLStreamWriter writer = StrolchXmlHelper.openXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				tx.streamResources().forEach(e -> e.accept(visitor));
				tx.streamOrders().forEach(e -> e.accept(visitor));
				tx.streamActivities().forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"").build();
	}

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

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/resources/xml")
	public Response exportResourcesToXml(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm)) {
				XMLStreamWriter writer = StrolchXmlHelper.openXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				tx.streamResources().forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_resources_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"").build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/xml")
	public Response exportOrdersToXml(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm)) {
				XMLStreamWriter writer = StrolchXmlHelper.openXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				tx.streamOrders().forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_orders_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"").build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/xml")
	public Response exportActivitiesToXml(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm)) {
				XMLStreamWriter writer = StrolchXmlHelper.openXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				tx.streamActivities().forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_activities_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"").build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}")
	public Response queryResourcesByType(@Context HttpServletRequest request, @BeanParam QueryData queryData,
			@PathParam("realm") String realm, @PathParam("type") String type,
			@QueryParam("overview") Boolean overview) {

		queryData.initializeUnsetFields();
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		// parse the query string
		ResourceSearch search = SearchBuilder.buildResourceSearch(queryData.getQuery(), type);

		// query the data
		RootElementSearchResult<Resource> result;
		long dataSetSize;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			dataSetSize = tx.getResourceMap().querySize(tx);
			result = search.search(tx);
		}

		// do ordering
		result = SearchBuilder.orderBy(result, queryData.getOrderBy(), queryData.isDescending());

		// build JSON response
		ResourceVisitor<JsonObject> visitor;
		if (overview == null || !overview) {
			visitor = new StrolchRootElementToJsonVisitor().withVersion().asResourceVisitor();
		} else {
			visitor = e -> {
				JsonObject jsonObject = new JsonObject();
				jsonObject.addProperty(Json.OBJECT_TYPE, e.getObjectType());
				jsonObject.addProperty(Json.ID, e.getId());
				jsonObject.addProperty(Json.NAME, e.getName());
				jsonObject.addProperty(Json.TYPE, e.getType());
				return jsonObject;
			};
		}
		JsonObject root = RestfulHelper.toJson(queryData, dataSetSize, result, visitor);

		// marshall result
		return Response.ok(toString(root)).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}")
	public Response queryOrdersByType(@Context HttpServletRequest request, @BeanParam QueryData queryData,
			@PathParam("realm") String realm, @PathParam("type") String type,
			@QueryParam("overview") Boolean overview) {

		queryData.initializeUnsetFields();
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		// parse the query string
		OrderSearch search = SearchBuilder.buildOrderSearch(queryData.getQuery(), type);

		// query the data
		RootElementSearchResult<Order> result;
		long dataSetSize;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			dataSetSize = tx.getOrderMap().querySize(tx);
			result = search.search(tx);
		}

		// do ordering
		result = SearchBuilder.orderBy(result, queryData.getOrderBy(), queryData.isDescending());

		// build JSON response
		OrderVisitor<JsonObject> visitor;
		if (overview == null || !overview) {
			visitor = new StrolchRootElementToJsonVisitor().withVersion().asOrderVisitor();
		} else {
			visitor = e -> {
				JsonObject jsonObject = new JsonObject();
				jsonObject.addProperty(Json.OBJECT_TYPE, e.getObjectType());
				jsonObject.addProperty(Json.ID, e.getId());
				jsonObject.addProperty(Json.NAME, e.getName());
				jsonObject.addProperty(Json.TYPE, e.getType());
				jsonObject.addProperty(Json.STATE, e.getState().name());
				jsonObject.addProperty(Json.DATE, ISO8601FormatFactory.getInstance().formatDate(e.getDate()));
				return jsonObject;
			};
		}
		JsonObject root = RestfulHelper.toJson(queryData, dataSetSize, result, visitor);

		// marshall result
		return Response.ok(toString(root)).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}")
	public Response queryActivitiesByType(@Context HttpServletRequest request, @BeanParam QueryData queryData,
			@PathParam("realm") String realm, @PathParam("type") String type,
			@QueryParam("overview") Boolean overview) {

		queryData.initializeUnsetFields();
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		// parse the query string
		ActivitySearch search = SearchBuilder.buildActivitySearch(queryData.getQuery(), type);

		// query the data
		RootElementSearchResult<Activity> result;
		long dataSetSize;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			dataSetSize = tx.getActivityMap().querySize(tx);
			result = search.search(tx);
		}

		// do ordering
		result = SearchBuilder.orderBy(result, queryData.getOrderBy(), queryData.isDescending());

		// build JSON response
		ActivityVisitor<JsonObject> visitor;
		if (overview == null || !overview) {
			visitor = new StrolchRootElementToJsonVisitor().withVersion().asActivityVisitor();
		} else {
			visitor = e -> {
				JsonObject jsonObject = new JsonObject();
				jsonObject.addProperty(Json.OBJECT_TYPE, e.getObjectType());
				jsonObject.addProperty(Json.ID, e.getId());
				jsonObject.addProperty(Json.NAME, e.getName());
				jsonObject.addProperty(Json.TYPE, e.getType());
				jsonObject.addProperty(Json.STATE, e.getState().name());
				jsonObject.addProperty(Json.TIME_ORDERING, e.getTimeOrdering().name());
				return jsonObject;
			};
		}
		JsonObject root = RestfulHelper.toJson(queryData, dataSetSize, result, visitor);

		// marshall result
		return Response.ok(toString(root)).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/resources/{type}/xml")
	public Response exportResourcesOfTypeToXml(@BeanParam QueryData queryData, @PathParam("realm") String realm,
			@PathParam("type") String type, @Context HttpServletRequest request) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		queryData.initializeUnsetFields();

		ResourceSearch search = SearchBuilder.buildResourceSearch(queryData.getQuery(), type);

		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm)) {

				XMLStreamWriter writer = StrolchXmlHelper.openXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				search.search(tx).forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_resources_" + type + "_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"").build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/{type}/xml")
	public Response exportOrdersOfTypeToXml(@BeanParam QueryData queryData, @PathParam("realm") String realm,
			@PathParam("type") String type, @Context HttpServletRequest request) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		queryData.initializeUnsetFields();

		OrderSearch search = SearchBuilder.buildOrderSearch(queryData.getQuery(), type);

		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm)) {

				XMLStreamWriter writer = StrolchXmlHelper.openXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				search.search(tx).forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_orders_" + type + "_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"").build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/{type}/xml")
	public Response exportActivitiesOfTypeToXml(@BeanParam QueryData queryData, @PathParam("realm") String realm,
			@PathParam("type") String type, @Context HttpServletRequest request) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		queryData.initializeUnsetFields();

		ActivitySearch search = SearchBuilder.buildActivitySearch(queryData.getQuery(), type);

		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm)) {

				XMLStreamWriter writer = StrolchXmlHelper.openXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				search.search(tx).forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_activities_" + type + "_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"").build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}/{id}")
	public Response getResourceAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") String flat) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Resource resource;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			resource = tx.getResourceBy(type, id);
		}
		if (resource == null) {
			throw new StrolchException(MessageFormat.format("No Resource exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor().withVersion();
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
			resource = tx.getResourceBy(type, id);
		}
		if (resource == null) {
			throw new StrolchException(MessageFormat.format("No Resource exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		String asXml = resource.accept(new StrolchElementToXmlStringVisitor());
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}/{id}")
	public Response getOrderAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") String flat) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Order order;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			order = tx.getOrderBy(type, id);
		}
		if (order == null) {
			throw new StrolchException(MessageFormat.format("No Order exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor().withVersion();
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
			order = tx.getOrderBy(type, id);
		}
		if (order == null) {
			throw new StrolchException(MessageFormat.format("No Order exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		String asXml = order.accept(new StrolchElementToXmlStringVisitor());
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}/{id}")
	public Response getActivityAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") String flat) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Activity activity;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			activity = tx.getActivityBy(type, id);
		}
		if (activity == null) {
			throw new StrolchException(MessageFormat.format("No Activity exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor().withVersion();
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
			activity = tx.getActivityBy(type, id);
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
	@Path("{realm}/resources/{type}/{id}")
	public Response updateResourceAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Resource resource = parseResourceFromXml(type, data);
		DBC.INTERIM.assertEquals("Posted id must be same as request!", id, resource.getId());

		UpdateResourceService svc = new UpdateResourceService();
		UpdateResourceArg arg = new UpdateResourceArg();
		arg.refreshUnknownVersion = true;
		arg.resource = resource;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = resource.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return ResponseUtil.toResponse(result);
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
		}

		DBC.INTERIM.assertEquals("Posted id must be same as request!", id, resource.getId());

		// prepare argument
		arg.refreshUnknownVersion = true;
		arg.resource = resource;
		arg.realm = realm;

		// do service
		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withVersion();
			if (flat)
				toJsonVisitor.flat();
			return Response.ok().entity(toString(resource.accept(toJsonVisitor))).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/{type}/{id}")
	public Response updateOrderAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Order order = parseOrderFromXml(type, data);
		DBC.INTERIM.assertEquals("Posted id must be same as request!", id, order.getId());

		UpdateOrderService svc = new UpdateOrderService();
		UpdateOrderArg arg = new UpdateOrderArg();
		arg.refreshUnknownVersion = true;
		arg.order = order;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = order.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return ResponseUtil.toResponse(result);
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
		}

		DBC.INTERIM.assertEquals("Posted id must be same as request!", id, order.getId());

		// prepare argument
		arg.refreshUnknownVersion = true;
		arg.order = order;
		arg.realm = realm;

		// do service
		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withVersion();
			if (flat)
				toJsonVisitor.flat();
			return Response.ok().entity(toString(order.accept(toJsonVisitor))).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/{type}/{id}")
	public Response updateActivityAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Activity activity = parseActivityFromXml(type, data);
		DBC.INTERIM.assertEquals("Posted id must be same as request!", id, activity.getId());

		UpdateActivityService svc = new UpdateActivityService();
		UpdateActivityArg arg = new UpdateActivityArg();
		arg.refreshUnknownVersion = true;
		arg.activity = activity;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = activity.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return ResponseUtil.toResponse(result);
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
		}

		DBC.INTERIM.assertEquals("Posted id must be same as request!", id, activity.getId());

		// prepare argument
		arg.refreshUnknownVersion = true;
		arg.activity = activity;
		arg.realm = realm;

		// do service
		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withVersion();
			if (flat)
				toJsonVisitor.flat();
			return Response.ok().entity(toString(activity.accept(toJsonVisitor))).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@POST
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/import")
	public Response importAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		File tempFile = null;
		try {

			tempFile = File.createTempFile("strolch_model_upload_", ".xml");
			try (FileWriter out = new FileWriter(tempFile)) {
				out.write(data);
			}

			XmlImportModelService svc = new XmlImportModelService();
			XmlImportModelArgument arg = new XmlImportModelArgument();
			arg.modelFileName = tempFile.getAbsolutePath();
			arg.allowInclude = false;
			arg.external = true;
			arg.addOrders = true;
			arg.addResources = true;
			arg.updateOrders = true;
			arg.updateResources = true;
			arg.orderTypes = Collections.emptySet();
			arg.resourceTypes = Collections.emptySet();
			arg.realm = realm;

			XmlImportModelResult svcResult = RestfulStrolchComponent.getInstance().getServiceHandler()
					.doService(cert, svc, arg);
			if (svcResult.isOk())
				return ResponseUtil.toResponse(MSG, svcResult.getStatistics().toString());
			return ResponseUtil.toResponse(svcResult);

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return ResponseUtil.toResponse(e);
		} finally {
			if (tempFile != null) {
				if (!tempFile.delete())
					logger.error("Failed to delete " + tempFile.getAbsolutePath());
			}
		}
	}

	@POST
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/resources")
	public Response addResourceAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Resource resource = parseResourceFromXml(null, data);

		AddResourceService svc = new AddResourceService();
		AddResourceArg arg = new AddResourceArg();
		arg.resource = resource;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = resource.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources")
	public Response addResourceAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		// parse from complete JSON
		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();
		ResourceFromJsonVisitor visitor = new ResourceFromJsonVisitor();
		Resource resource = visitor.visit(jsonObject);

		AddResourceService svc = new AddResourceService();
		AddResourceArg arg = new AddResourceArg();
		arg.resource = resource;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withVersion();
			return Response.ok().entity(toString(resource.accept(toJsonVisitor))).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}")
	public Response addResourceAsJsonFlat(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @QueryParam("flat") String flatS, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		boolean flat = Boolean.parseBoolean(flatS);

		Resource resource = parseNewResourceFromJson(cert, realm, type, data, flat);

		AddResourceService svc = new AddResourceService();
		AddResourceArg arg = new AddResourceArg();
		arg.resource = resource;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withVersion();
			if (flat)
				toJsonVisitor.flat();
			return Response.ok().entity(toString(resource.accept(toJsonVisitor))).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@POST
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/orders")
	public Response addOrderAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Order order = parseOrderFromXml(null, data);

		AddOrderService svc = new AddOrderService();
		AddOrderArg arg = new AddOrderArg();
		arg.order = order;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = order.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders")
	public Response addOrderAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		// parse from complete JSON
		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();
		OrderFromJsonVisitor visitor = new OrderFromJsonVisitor();
		Order order = visitor.visit(jsonObject);

		AddOrderService svc = new AddOrderService();
		AddOrderArg arg = new AddOrderArg();
		arg.order = order;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withVersion();
			return Response.ok().entity(toString(order.accept(toJsonVisitor))).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}")
	public Response addOrderAsJsonFlat(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @QueryParam("flat") String flatS, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		boolean flat = Boolean.parseBoolean(flatS);

		Order order = parseNewOrderFromJson(cert, realm, type, data, flat);

		AddOrderService svc = new AddOrderService();
		AddOrderArg arg = new AddOrderArg();
		arg.order = order;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withVersion();
			if (flat)
				toJsonVisitor.flat();
			return Response.ok().entity(toString(order.accept(toJsonVisitor))).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@POST
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/activities")
	public Response addActivityAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Activity activity = parseActivityFromXml(null, data);

		AddActivityService svc = new AddActivityService();
		AddActivityArg arg = new AddActivityArg();
		arg.activity = activity;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = activity.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities")
	public Response addActivityAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		// parse from complete JSON
		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();
		ActivityFromJsonVisitor visitor = new ActivityFromJsonVisitor();
		Activity activity = visitor.visit(jsonObject);

		AddActivityService svc = new AddActivityService();
		AddActivityArg arg = new AddActivityArg();
		arg.activity = activity;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withVersion();
			return Response.ok().entity(toString(activity.accept(toJsonVisitor))).build();
		}

		return ResponseUtil.toResponse(result);
	}

	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}")
	public Response addActivityAsJsonFlat(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @QueryParam("flat") String flatS, String data) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		boolean flat = Boolean.parseBoolean(flatS);

		Activity activity = parseNewActivityFromJson(cert, realm, type, data, flat);

		AddActivityService svc = new AddActivityService();
		AddActivityArg arg = new AddActivityArg();
		arg.activity = activity;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withVersion();
			if (flat)
				toJsonVisitor.flat();
			return Response.ok().entity(toString(activity.accept(toJsonVisitor))).build();
		}

		return ResponseUtil.toResponse(result);
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
		return ResponseUtil.toResponse(result);
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
		return ResponseUtil.toResponse(result);
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
		return ResponseUtil.toResponse(result);
	}

	private Resource parseResourceFromXml(String type, String data) {
		Resource resource;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getResources().size() == 0)
				throw new StrolchPersistenceException(
						"No Resource parsed from xml value" + (StringHelper.isNotEmpty(type) ?
								" for type " + type :
								""));
			if (listener.getResources().size() > 1)
				throw new StrolchPersistenceException(
						"Multiple Resources parsed from xml value" + (StringHelper.isNotEmpty(type) ?
								" for type " + type :
								""));

			resource = listener.getResources().get(0);
			resource.setVersion(null);

			DBC.INTERIM.assertEquals("Posted type must be same as request!", type, resource.getType());

		} catch (Exception e) {
			throw new StrolchPersistenceException(
					"Failed to extract Resource from xml value" + (StringHelper.isNotEmpty(type) ?
							" for type " + type :
							""), e);
		}
		return resource;
	}

	private Order parseOrderFromXml(String type, String data) {
		Order order;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getOrders().size() == 0)
				throw new StrolchPersistenceException(
						"No Order parsed from xml value" + (StringHelper.isNotEmpty(type) ? " for type " + type : ""));
			if (listener.getOrders().size() > 1)
				throw new StrolchPersistenceException(
						"Multiple Orders parsed from xml value" + (StringHelper.isNotEmpty(type) ?
								" for type " + type :
								""));

			order = listener.getOrders().get(0);
			order.setVersion(null);

			DBC.INTERIM.assertEquals("Posted type must be same as request!", type, order.getType());

		} catch (Exception e) {
			throw new StrolchPersistenceException(
					"Failed to extract Order from xml value" + (StringHelper.isNotEmpty(type) ?
							" for type " + type :
							""), e);
		}
		return order;
	}

	private Activity parseActivityFromXml(String type, String data) {
		Activity activity;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getActivities().size() == 0)
				throw new StrolchPersistenceException(
						"No Activity parsed from xml value" + (StringHelper.isNotEmpty(type) ?
								" for type " + type :
								""));
			if (listener.getActivities().size() > 1)
				throw new StrolchPersistenceException(
						"Multiple Activities parsed from xml value" + (StringHelper.isNotEmpty(type) ?
								" for type " + type :
								""));

			activity = listener.getActivities().get(0);
			activity.setVersion(null);

			DBC.INTERIM.assertEquals("Posted type must be same as request!", type, activity.getType());

		} catch (Exception e) {
			throw new StrolchPersistenceException(
					"Failed to extract Activity from xml value" + (StringHelper.isNotEmpty(type) ?
							" for type " + type :
							""), e);
		}
		return activity;
	}

	private Resource parseNewResourceFromJson(Certificate cert, String realm, String type, String data, boolean flat) {

		// parse JSON string
		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();
		Resource resource;
		if (flat) {

			// parse from flat JSON
			try (StrolchTransaction tx = openTx(cert, realm)) {
				resource = tx.getResourceTemplate(type, true);
			}
			resource.setId(jsonObject.get(Json.ID).getAsString());
			new FromFlatJsonVisitor().visit(resource, jsonObject);

		} else {

			// parse from complete JSON
			ResourceFromJsonVisitor visitor = new ResourceFromJsonVisitor();
			resource = visitor.visit(jsonObject);
			resource.setVersion(null);

			DBC.INTERIM.assertEquals("Posted type must be same as request!", type, resource.getType());
		}

		return resource;
	}

	private Order parseNewOrderFromJson(Certificate cert, String realm, String type, String data, boolean flat) {

		// parse JSON string
		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();
		Order order;
		if (flat) {

			// parse from flat JSON
			try (StrolchTransaction tx = openTx(cert, realm)) {
				order = tx.getOrderTemplate(type, true);
			}
			order.setId(jsonObject.get(Json.ID).getAsString());
			new FromFlatJsonVisitor().visit(order, jsonObject);

		} else {

			// parse from complete JSON
			OrderFromJsonVisitor visitor = new OrderFromJsonVisitor();
			order = visitor.visit(jsonObject);
			order.setVersion(null);

			DBC.INTERIM.assertEquals("Posted type must be same as request!", type, order.getType());
		}

		return order;
	}

	private Activity parseNewActivityFromJson(Certificate cert, String realm, String type, String data, boolean flat) {

		// parse JSON string
		JsonObject jsonObject = new JsonParser().parse(data).getAsJsonObject();
		Activity activity;
		if (flat) {

			// parse from flat JSON
			try (StrolchTransaction tx = openTx(cert, realm)) {
				activity = tx.getActivityTemplate(type, true);
			}
			activity.setId(jsonObject.get(Json.ID).getAsString());
			new FromFlatJsonVisitor().visit(activity, jsonObject);

		} else {

			// parse from complete JSON
			ActivityFromJsonVisitor visitor = new ActivityFromJsonVisitor();
			activity = visitor.visit(jsonObject);
			activity.setVersion(null);

			DBC.INTERIM.assertEquals("Posted type must be same as request!", type, activity.getType());
		}

		return activity;
	}
}
