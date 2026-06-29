/*
 * Copyright (c) 2015-2025 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.StreamingOutput;
import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Locator;
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
import li.strolch.rest.model.QueryData;
import li.strolch.rest.model.ServiceResultResponse;
import li.strolch.search.*;
import li.strolch.service.*;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.helper.TempFileOptions;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.InputSource;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.nio.file.Files;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Set;

import static java.util.Collections.emptySet;
import static java.util.Collections.sort;
import static li.strolch.rest.StrolchRestfulConstants.MSG;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;
import static li.strolch.rest.helper.ResponseUtil.toResponse;
import static li.strolch.rest.helper.RestfulHelper.toJson;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE_INSPECTOR;
import static li.strolch.search.SearchBuilder.orderBy;
import static li.strolch.utils.helper.ExceptionHelper.getCallerMethod;
import static li.strolch.utils.helper.ExceptionHelper.getCallerMethodNoClass;
import static li.strolch.utils.helper.FileHelper.getTempFile;
import static li.strolch.utils.helper.TempFileOptions.APPEND_MILLIS;
import static li.strolch.utils.helper.XmlHelper.getSaxParser;

/**
 * The RESTful inspector for Strolch. It allows to inspect the realms, and their respective elements. Supporting
 * querying and retrieving, in multiple formats: XML, JSON and flat JSON
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/inspector")
@Tag(name = "Inspector", description = "Provides operations for inspecting realms and elements in various formats.")
public class InspectorResource {

	private static final Logger logger = LoggerFactory.getLogger(InspectorResource.class);

	private static Certificate validateCertificate(HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();
		rest.validate(cert).validateAction(PRIVILEGE_INSPECTOR, getCallerMethodNoClass(2));
		return cert;
	}

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().openTx(certificate, realm, getCallerMethod(2));
	}

	private StrolchTransaction openTx(Certificate certificate, String realm, String action) {
		return RestfulStrolchComponent.getInstance().openTx(certificate, realm, action);
	}

	@Operation(summary = "Get agent overview",
			description = "Retrieves an overview of the available realms and their element counts.", responses = {
			@ApiResponse(responseCode = "200", description = "Agent overview retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAgentOverview(@Context HttpServletRequest request) {
		Certificate cert = validateCertificate(request);

		JsonObject agentOverview = new JsonObject();
		JsonArray realmsArr = new JsonArray();
		agentOverview.add(Tags.Json.REALMS, realmsArr);

		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		Set<String> realmNames = container.getRealmNames();
		for (String realmName : realmNames) {

			JsonObject realmJ = new JsonObject();

			try (StrolchTransaction tx = openTx(cert, realmName)) {
				long size = 0;
				size += tx.getResourceCount();
				size += tx.getOrderCount();

				realmJ.addProperty(Tags.Json.NAME, realmName);
				realmJ.addProperty(Tags.Json.SIZE, size);

				realmsArr.add(realmJ);
			}
		}

		return Response.ok().entity(agentOverview.toString()).build();
	}

	@Operation(summary = "Get realm overview", description = "Retrieves detailed information about a specific realm.",
			responses = {@ApiResponse(responseCode = "200", description = "Realm overview retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}")
	public Response getRealmOverview(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = validateCertificate(request);

		JsonObject realmDetailJ = new JsonObject();
		JsonArray elementMapsArr = new JsonArray();
		realmDetailJ.add(Tags.Json.ELEMENT_MAPS, elementMapsArr);

		try (StrolchTransaction tx = openTx(cert, realm)) {

			{
				ResourceMap resourceMap = tx.getResourceMap();
				JsonObject elementMapJ = new JsonObject();
				elementMapJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.RESOURCE);
				elementMapJ.addProperty(Tags.Json.NR_OF_ELEMENTS, tx.getResourceCount());
				JsonArray typesJ = new JsonArray();
				resourceMap.getTypes(tx).forEach(typesJ::add);
				elementMapJ.add(Tags.Json.TYPES, typesJ);

				elementMapsArr.add(elementMapJ);
			}

			{
				OrderMap orderMap = tx.getOrderMap();
				JsonObject elementMapJ = new JsonObject();
				elementMapJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ORDER);
				elementMapJ.addProperty(Tags.Json.NR_OF_ELEMENTS, tx.getOrderCount());
				JsonArray typesJ = new JsonArray();
				orderMap.getTypes(tx).forEach(typesJ::add);
				elementMapJ.add(Tags.Json.TYPES, typesJ);

				elementMapsArr.add(elementMapJ);
			}

			{
				ActivityMap activityMap = tx.getActivityMap();
				JsonObject elementMapJ = new JsonObject();
				elementMapJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ACTIVITY);
				elementMapJ.addProperty(Tags.Json.NR_OF_ELEMENTS, tx.getActivityCount());
				JsonArray typesJ = new JsonArray();
				activityMap.getTypes(tx).forEach(typesJ::add);
				elementMapJ.add(Tags.Json.TYPES, typesJ);

				elementMapsArr.add(elementMapJ);
			}
		}

		return Response.ok().entity(realmDetailJ.toString()).build();
	}

	@Operation(summary = "Export realm data as XML", description = "Exports all elements of a realm in XML format.",
			responses = {@ApiResponse(responseCode = "200", description = "Realm data exported successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/xml")
	public Response exportRealmToXml(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = validateCertificate(request);

		String action = getCallerMethod(1);
		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm, action)) {
				XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(stream);
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
		return Response
				.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
				.build();
	}

	@Operation(summary = "Get resources overview", description = "Retrieves an overview of resources within a realm.",
			responses = {@ApiResponse(responseCode = "200", description = "Resources overview retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources")
	public Response getResourcesOverview(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = validateCertificate(request);

		JsonObject mapOverview = new JsonObject();

		try (StrolchTransaction tx = openTx(cert, realm)) {
			ResourceMap resourceMap = tx.getResourceMap();

			mapOverview.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.RESOURCE);
			mapOverview.addProperty(Tags.Json.SIZE, tx.getResourceCount());

			JsonArray typeArrJ = new JsonArray();
			mapOverview.add(Tags.Json.TYPES, typeArrJ);

			List<String> types = new ArrayList<>(resourceMap.getTypes(tx));
			sort(types);
			types.forEach(type -> {

				JsonObject typeJ = new JsonObject();
				typeJ.addProperty(Tags.Json.TYPE, type);
				typeJ.addProperty(Tags.Json.SIZE, tx.getResourceCount(type));

				typeArrJ.add(typeJ);
			});
		}

		return Response.ok().entity(mapOverview.toString()).build();
	}

	@Operation(summary = "Get orders overview", description = "Retrieves an overview of orders within a realm.",
			responses = {@ApiResponse(responseCode = "200", description = "Orders overview retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders")
	public Response getOrdersOverview(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = validateCertificate(request);

		JsonObject mapOverview = new JsonObject();

		try (StrolchTransaction tx = openTx(cert, realm)) {
			OrderMap orderMap = tx.getOrderMap();

			mapOverview.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ORDER);
			mapOverview.addProperty(Tags.Json.SIZE, tx.getOrderCount());

			JsonArray typeArrJ = new JsonArray();
			mapOverview.add(Tags.Json.TYPES, typeArrJ);

			List<String> types = new ArrayList<>(orderMap.getTypes(tx));
			sort(types);
			types.forEach(type -> {

				JsonObject typeJ = new JsonObject();
				typeJ.addProperty(Tags.Json.TYPE, type);
				typeJ.addProperty(Tags.Json.SIZE, tx.getOrderCount(type));

				typeArrJ.add(typeJ);
			});
		}

		return Response.ok().entity(mapOverview.toString()).build();
	}

	@Operation(summary = "Get activities overview", description = "Retrieves an overview of activities within a realm.",
			responses = {@ApiResponse(responseCode = "200", description = "Activities overview retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities")
	public Response getActivitiesOverview(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = validateCertificate(request);

		JsonObject mapOverview = new JsonObject();

		try (StrolchTransaction tx = openTx(cert, realm)) {
			ActivityMap activityMap = tx.getActivityMap();

			mapOverview.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ACTIVITY);
			mapOverview.addProperty(Tags.Json.SIZE, tx.getActivityCount());

			JsonArray typeArrJ = new JsonArray();
			mapOverview.add(Tags.Json.TYPES, typeArrJ);

			List<String> types = new ArrayList<>(activityMap.getTypes(tx));
			sort(types);
			types.forEach(type -> {

				JsonObject typeJ = new JsonObject();
				typeJ.addProperty(Tags.Json.TYPE, type);
				typeJ.addProperty(Tags.Json.SIZE, tx.getActivityCount(type));

				typeArrJ.add(typeJ);
			});
		}

		return Response.ok().entity(mapOverview.toString()).build();
	}

	@Operation(summary = "Export resources as XML", description = "Exports all resources of a realm in XML format.",
			responses = {@ApiResponse(responseCode = "200", description = "Resources exported successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/resources/xml")
	public Response exportResourcesToXml(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = validateCertificate(request);

		String action = getCallerMethod(1);
		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm, action)) {
				XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				tx.streamResources().forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_resources_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response
				.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
				.build();
	}

	@Operation(summary = "Export orders as XML", description = "Exports all orders of a realm in XML format.",
			responses = {@ApiResponse(responseCode = "200", description = "Orders exported successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/xml")
	public Response exportOrdersToXml(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = validateCertificate(request);

		String action = getCallerMethod(1);
		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm, action)) {
				XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				tx.streamOrders().forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_orders_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response
				.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
				.build();
	}

	@Operation(summary = "Export activities as XML", description = "Exports all activities of a realm in XML format.",
			responses = {@ApiResponse(responseCode = "200", description = "Activities exported successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/xml")
	public Response exportActivitiesToXml(@Context HttpServletRequest request, @PathParam("realm") String realm) {

		Certificate cert = validateCertificate(request);

		String action = getCallerMethod(1);
		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm, action)) {
				XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				tx.streamActivities().forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_activities_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response
				.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
				.build();
	}

	@Operation(summary = "Query resources by type",
			description = "Retrieves resources of a specific type within a realm.", responses = {
			@ApiResponse(responseCode = "200", description = "Resources retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}")
	public Response queryResourcesByType(@Context HttpServletRequest request, @BeanParam QueryData queryData,
			@PathParam("realm") String realm, @PathParam("type") String type,
			@QueryParam("overview") boolean overview) {

		queryData.initializeUnsetFields();
		Certificate cert = validateCertificate(request);

		// parse the query string
		ResourceSearch search = SearchBuilder.buildResourceSearch(queryData.getQuery(), type);

		// query the data
		RootElementSearchResult<Resource> result;
		long dataSetSize;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			dataSetSize = tx.getResourceCount(type);
			result = search.search(tx);
		}

		// do ordering
		result = orderBy(result, queryData.getOrderBy(), queryData.isDescending());

		// build JSON response
		ResourceVisitor<JsonObject> visitor;
		if (!overview) {
			visitor = new StrolchRootElementToJsonVisitor().withLocator().asResourceVisitor();
		} else {
			visitor = e -> {
				JsonObject jsonObject = new JsonObject();
				jsonObject.addProperty(Json.OBJECT_TYPE, e.getObjectType());
				jsonObject.addProperty(Json.ID, e.getId());
				jsonObject.addProperty(Json.NAME, e.getName());
				jsonObject.addProperty(Json.TYPE, e.getType());
				jsonObject.addProperty(Json.LOCATOR, e.getLocator().toString());
				return jsonObject;
			};
		}
		JsonObject root = toJson(queryData, dataSetSize, result, visitor);

		// marshall result
		return Response.ok(root.toString()).build();
	}

	@Operation(summary = "Query orders by type", description = "Retrieves orders of a specific type within a realm.",
			responses = {@ApiResponse(responseCode = "200", description = "Orders retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}")
	public Response queryOrdersByType(@Context HttpServletRequest request, @BeanParam QueryData queryData,
			@PathParam("realm") String realm, @PathParam("type") String type,
			@QueryParam("overview") boolean overview) {

		queryData.initializeUnsetFields();
		Certificate cert = validateCertificate(request);

		// parse the query string
		OrderSearch search = SearchBuilder.buildOrderSearch(queryData.getQuery(), type);

		// query the data
		RootElementSearchResult<Order> result;
		long dataSetSize;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			dataSetSize = tx.getOrderCount(type);
			result = search.search(tx);
		}

		// do ordering
		result = orderBy(result, queryData.getOrderBy(), queryData.isDescending());

		// build JSON response
		OrderVisitor<JsonObject> visitor;
		if (!overview) {
			visitor = new StrolchRootElementToJsonVisitor().withLocator().asOrderVisitor();
		} else {
			visitor = e -> {
				JsonObject jsonObject = new JsonObject();
				jsonObject.addProperty(Json.OBJECT_TYPE, e.getObjectType());
				jsonObject.addProperty(Json.ID, e.getId());
				jsonObject.addProperty(Json.NAME, e.getName());
				jsonObject.addProperty(Json.TYPE, e.getType());
				jsonObject.addProperty(Json.LOCATOR, e.getLocator().toString());
				jsonObject.addProperty(Json.STATE, e.getState().name());
				jsonObject.addProperty(Json.DATE, ISO8601FormatFactory.getInstance().formatDate(e.getDate()));
				return jsonObject;
			};
		}
		JsonObject root = toJson(queryData, dataSetSize, result, visitor);

		// marshall result
		return Response.ok(root.toString()).build();
	}

	@Operation(summary = "Query activities by type",
			description = "Retrieves activities of a specific type within a realm.", responses = {
			@ApiResponse(responseCode = "200", description = "Activities retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}")
	public Response queryActivitiesByType(@Context HttpServletRequest request, @BeanParam QueryData queryData,
			@PathParam("realm") String realm, @PathParam("type") String type,
			@QueryParam("overview") boolean overview) {

		queryData.initializeUnsetFields();
		Certificate cert = validateCertificate(request);

		// parse the query string
		ActivitySearch search = SearchBuilder.buildActivitySearch(queryData.getQuery(), type);

		// query the data
		RootElementSearchResult<Activity> result;
		long dataSetSize;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			dataSetSize = tx.getActivityCount(type);
			result = search.search(tx);
		}

		// do ordering
		result = orderBy(result, queryData.getOrderBy(), queryData.isDescending());

		// build JSON response
		ActivityVisitor<JsonObject> visitor;
		if (!overview) {
			visitor = new StrolchRootElementToJsonVisitor().withLocator().asActivityVisitor();
		} else {
			visitor = e -> {
				JsonObject jsonObject = new JsonObject();
				jsonObject.addProperty(Json.OBJECT_TYPE, e.getObjectType());
				jsonObject.addProperty(Json.ID, e.getId());
				jsonObject.addProperty(Json.NAME, e.getName());
				jsonObject.addProperty(Json.TYPE, e.getType());
				jsonObject.addProperty(Json.LOCATOR, e.getLocator().toString());
				jsonObject.addProperty(Json.STATE, e.getState().name());
				jsonObject.addProperty(Json.TIME_ORDERING, e.getTimeOrdering().name());
				return jsonObject;
			};
		}
		JsonObject root = toJson(queryData, dataSetSize, result, visitor);

		// marshall result
		return Response.ok(root.toString()).build();
	}

	@Operation(summary = "Export resources of a specific type as XML",
			description = "Exports all resources of a given type within a realm in XML format.", responses = {
			@ApiResponse(responseCode = "200", description = "Resources exported successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/resources/{type}/xml")
	public Response exportResourcesOfTypeToXml(@BeanParam QueryData queryData, @PathParam("realm") String realm,
			@PathParam("type") String type, @Context HttpServletRequest request) {

		Certificate cert = validateCertificate(request);

		queryData.initializeUnsetFields();

		ResourceSearch search = SearchBuilder.buildResourceSearch(queryData.getQuery(), type);

		String action = getCallerMethod(1);
		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm, action)) {

				XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				search.search(tx).forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_resources_" + type + "_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response
				.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
				.build();
	}

	@Operation(summary = "Export orders of a specific type as XML",
			description = "Exports all orders of a given type within a realm in XML format.", responses = {
			@ApiResponse(responseCode = "200", description = "Orders exported successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/{type}/xml")
	public Response exportOrdersOfTypeToXml(@BeanParam QueryData queryData, @PathParam("realm") String realm,
			@PathParam("type") String type, @Context HttpServletRequest request) {

		Certificate cert = validateCertificate(request);

		queryData.initializeUnsetFields();

		OrderSearch search = SearchBuilder.buildOrderSearch(queryData.getQuery(), type);

		String action = getCallerMethod(1);
		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm, action)) {

				XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				search.search(tx).forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_orders_" + type + "_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response
				.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
				.build();
	}

	@Operation(summary = "Export activities of a specific type as XML",
			description = "Exports all activities of a given type within a realm in XML format.", responses = {
			@ApiResponse(responseCode = "200", description = "Activities exported successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/{type}/xml")
	public Response exportActivitiesOfTypeToXml(@BeanParam QueryData queryData, @PathParam("realm") String realm,
			@PathParam("type") String type, @Context HttpServletRequest request) {

		Certificate cert = validateCertificate(request);

		queryData.initializeUnsetFields();

		ActivitySearch search = SearchBuilder.buildActivitySearch(queryData.getQuery(), type);

		String action = getCallerMethod(1);
		StreamingOutput streamingOutput = stream -> {
			try (StrolchTransaction tx = openTx(cert, realm, action)) {

				XMLStreamWriter writer = StrolchXmlHelper.prepareXmlStreamWriter(stream);
				StrolchElementToSaxWriterVisitor visitor = new StrolchElementToSaxWriterVisitor(writer);

				search.search(tx).forEach(e -> e.accept(visitor));

				writer.writeEndDocument();
				stream.flush();

			} catch (XMLStreamException e) {
				throw new IllegalStateException("Failed to write XML to " + stream, e);
			}
		};

		String fileName = "strolch_export_activities_" + type + "_" + realm + "_" + System.currentTimeMillis() + ".xml";
		return Response
				.ok(streamingOutput, MediaType.APPLICATION_XML)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
				.build();
	}

	@Operation(summary = "Get resource as JSON", description = "Retrieves a specific resource in JSON format.",
			responses = {@ApiResponse(responseCode = "200", description = "Resource retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "404", description = "Resource not found."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}/{id}")
	public Response getResourceAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") boolean flat) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		Resource resource;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			resource = tx.getResourceBy(type, id);
		}
		if (resource == null) {
			throw new StrolchException(MessageFormat.format("No Resource exists for {0}/{1}", type, id));
		}

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
		if (flat)
			visitor.flat();

		JsonElement jsonElement = resource.accept(visitor);
		return Response.ok().entity(jsonElement.toString()).build();
	}

	private static String decodeId(String id) {
		id = id.startsWith("b64_") ? new String(Base64.getDecoder().decode(id.substring(4))) : id;
		return id;
	}

	@Operation(summary = "Get resource as XML", description = "Retrieves a specific resource in XML format.",
			responses = {@ApiResponse(responseCode = "200", description = "Resource retrieved successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "404", description = "Resource not found."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/resources/{type}/{id}")
	public Response getResourceAsXml(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		Resource resource;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			resource = tx.getResourceBy(type, id);
		}
		if (resource == null) {
			throw new StrolchException(MessageFormat.format("No Resource exists for {0}/{1}", type, id));
		}

		String asXml = resource.accept(new StrolchElementToXmlStringVisitor());
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@Operation(summary = "Get order as JSON", description = "Retrieves a specific order in JSON format.", responses = {
			@ApiResponse(responseCode = "200", description = "Order retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
			@ApiResponse(responseCode = "404", description = "Order not found."),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}/{id}")
	public Response getOrderAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") boolean flat) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		Order order;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			order = tx.getOrderBy(type, id);
		}
		if (order == null) {
			throw new StrolchException(MessageFormat.format("No Order exists for {0}/{1}", type, id));
		}

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
		if (flat)
			visitor.flat();
		JsonElement jsonElement = order.accept(visitor);
		return Response.ok().entity(jsonElement.toString()).build();
	}

	@Operation(summary = "Get order as XML", description = "Retrieves a specific order in XML format.", responses = {
			@ApiResponse(responseCode = "200", description = "Order retrieved successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			@ApiResponse(responseCode = "404", description = "Order not found."),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/{type}/{id}")
	public Response getOrderAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		Order order;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			order = tx.getOrderBy(type, id);
		}
		if (order == null) {
			throw new StrolchException(MessageFormat.format("No Order exists for {0}/{1}", type, id));
		}

		String asXml = order.accept(new StrolchElementToXmlStringVisitor());
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@Operation(summary = "Get activity as JSON", description = "Retrieves a specific activity in JSON format.",
			responses = {@ApiResponse(responseCode = "200", description = "Activity retrieved successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "404", description = "Activity not found."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}/{id}")
	public Response getActivityAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") boolean flat) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		Activity activity;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			activity = tx.getActivityBy(type, id);
		}
		if (activity == null) {
			throw new StrolchException(MessageFormat.format("No Activity exists for {0}/{1}", type, id));
		}

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
		if (flat)
			visitor.flat();
		JsonElement jsonElement = activity.accept(visitor);
		return Response.ok().entity(jsonElement.toString()).build();
	}

	@Operation(summary = "Get activity as XML", description = "Retrieves a specific activity in XML format.",
			responses = {@ApiResponse(responseCode = "200", description = "Activity retrieved successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "404", description = "Activity not found."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/{type}/{id}")
	public Response getActivityAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		Activity activity;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			activity = tx.getActivityBy(type, id);
		}
		if (activity == null) {
			throw new StrolchException(MessageFormat.format("No Activity exists for {0}/{1}", type, id));
		}

		String asXml = activity.accept(new StrolchElementToXmlStringVisitor());
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@Operation(summary = "Update a resource as XML", description = "Updates a specific resource in XML format.",
			requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			responses = {@ApiResponse(responseCode = "200", description = "Resource updated successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "400", description = "Invalid XML format or mismatched ID."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "404", description = "Resource not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/resources/{type}/{id}")
	public Response updateResourceAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, String data) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		Resource resource = parseResourceFromXml(type, data);
		DBC.INTERIM.assertEquals("Posted id must be same as request!", id, resource.getId());

		UpdateResourceService svc = new UpdateResourceService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.refreshUnknownVersion = true;
		arg.rootElement = resource;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = resource.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Update a resource as JSON", description = "Updates a specific resource in JSON format.",
			responses = {@ApiResponse(responseCode = "200", description = "Resource updated successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "400", description = "Invalid JSON format or mismatched ID."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "404", description = "Resource not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}/{id}")
	public Response updateResourceAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") boolean flat, String data) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		UpdateResourceService svc = new UpdateResourceService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();

		// parse JSON string
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
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
		arg.rootElement = resource;
		arg.realm = realm;

		// do service
		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
			if (flat)
				toJsonVisitor.flat();
			JsonElement jsonElement = resource.accept(toJsonVisitor);
			return Response.ok().entity(jsonElement.toString()).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Update an order as XML", description = "Updates a specific order in XML format.",
			requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			responses = {@ApiResponse(responseCode = "200", description = "Order updated successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "400", description = "Invalid XML format or mismatched ID."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "404", description = "Order not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/orders/{type}/{id}")
	public Response updateOrderAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, String data) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		Order order = parseOrderFromXml(type, data);
		DBC.INTERIM.assertEquals("Posted id must be same as request!", id, order.getId());

		UpdateOrderService svc = new UpdateOrderService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.refreshUnknownVersion = true;
		arg.rootElement = order;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = order.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Update an order as JSON", description = "Updates a specific order in JSON format.",
			responses = {@ApiResponse(responseCode = "200", description = "Order updated successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "400", description = "Invalid JSON format or mismatched ID."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "404", description = "Order not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}/{id}")
	public Response updateOrderAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") boolean flat, String data) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		UpdateOrderService svc = new UpdateOrderService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();

		// parse JSON string
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
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
		arg.rootElement = order;
		arg.realm = realm;

		// do service
		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
			if (flat)
				toJsonVisitor.flat();
			JsonElement jsonElement = order.accept(toJsonVisitor);
			return Response.ok().entity(jsonElement.toString()).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Update an activity as XML", description = "Updates a specific activity in XML format.",
			requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			responses = {@ApiResponse(responseCode = "200", description = "Activity updated successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "400", description = "Invalid XML format or mismatched ID."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "404", description = "Activity not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/activities/{type}/{id}")
	public Response updateActivityAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, String data) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		Activity activity = parseActivityFromXml(type, data);
		DBC.INTERIM.assertEquals("Posted id must be same as request!", id, activity.getId());

		UpdateActivityService svc = new UpdateActivityService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.refreshUnknownVersion = true;
		arg.rootElement = activity;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = activity.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Update an activity as JSON", description = "Updates a specific activity in JSON format.",
			responses = {@ApiResponse(responseCode = "200", description = "Activity updated successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "400", description = "Invalid JSON format or mismatched ID."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "404", description = "Activity not found."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}/{id}")
	public Response updateActivityAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @PathParam("id") String id, @QueryParam("flat") boolean flat, String data) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		UpdateActivityService svc = new UpdateActivityService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();

		// parse JSON string
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
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
		arg.rootElement = activity;
		arg.realm = realm;

		// do service
		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
			if (flat)
				toJsonVisitor.flat();
			JsonElement jsonElement = activity.accept(toJsonVisitor);
			return Response.ok().entity(jsonElement.toString()).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Import model data as XML",
			description = "Imports model data into the system from an XML file, with various options for adding or updating resources, orders, and activities.",
			requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			responses = {@ApiResponse(responseCode = "200", description = "Import successful.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
					@ApiResponse(responseCode = "400", description = "Invalid XML format."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/import")
	public Response importAsXml(@Context HttpServletRequest request, //
			@PathParam("realm") String realm, //
			@QueryParam("failOnUpdate") boolean failOnUpdate, //
			@QueryParam("addResources") boolean addResources, //
			@QueryParam("addOrders") boolean addOrders, //
			@QueryParam("addActivities") boolean addActivities, //
			@QueryParam("updateResources") boolean updateResources, //
			@QueryParam("updateOrders") boolean updateOrders, //
			@QueryParam("updateActivities") boolean updateActivities, //
			String data) {

		Certificate cert = validateCertificate(request);

		File tempFile = null;
		try {

			File tempPath = new File(
					RestfulStrolchComponent.getInstance().getAgent().getRuntimeConfiguration().getTempPath(),
					"strolch_model_upload");
			if (!tempPath.exists() && !tempPath.mkdirs())
				throw new RuntimeException("Could not create temp path: " + tempPath.getName());
			Set<TempFileOptions> options = Set.of(APPEND_MILLIS);
			tempFile = getTempFile(tempPath, "strolch_model_upload_", ".xml", options);

			try (FileWriter out = new FileWriter(tempFile)) {
				out.write(data);
			}

			XmlImportModelService svc = new XmlImportModelService();
			XmlImportModelArgument arg = new XmlImportModelArgument();
			arg.modelFileName = tempFile.getAbsolutePath();
			arg.allowInclude = false;
			arg.external = true;
			arg.failOnUpdate = failOnUpdate;
			arg.addResources = addResources;
			arg.addOrders = addOrders;
			arg.addActivities = addActivities;
			arg.updateResources = updateResources;
			arg.updateOrders = updateOrders;
			arg.updateActivities = updateActivities;
			arg.orderTypes = emptySet();
			arg.resourceTypes = emptySet();
			arg.activityTypes = emptySet();
			arg.realm = realm;

			XmlImportModelResult svcResult = getServiceHandler().doService(cert, svc, arg);
			if (svcResult.isOk())
				return toResponse(MSG, svcResult.getStatistics().toString());
			return toResponse(svcResult);

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return toResponse(e);
		} finally {
			if (tempFile != null) {
				try {
					Files.deleteIfExists(tempFile.toPath());
				} catch (IOException e) {
					logger.error("Failed to delete {}", tempFile.getAbsolutePath());
				}
			}
		}
	}

	private ServiceHandler getServiceHandler() {
		return RestfulStrolchComponent.getInstance().getServiceHandler();
	}

	@Operation(summary = "Add a resource as XML", description = "Adds a new resource to the system using XML format.",
			requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			responses = {@ApiResponse(responseCode = "200", description = "Resource added successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "400", description = "Invalid XML format."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/resources")
	public Response addResourceAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			String data) {

		Certificate cert = validateCertificate(request);

		Resource resource = parseResourceFromXml(null, data);

		AddResourceService svc = new AddResourceService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.rootElement = resource;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = resource.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Add a resource as JSON", description = "Adds a new resource to the system using JSON format.",
			responses = {@ApiResponse(responseCode = "200", description = "Resource added successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "400", description = "Invalid JSON format."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources")
	public Response addResourceAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			String data) {

		Certificate cert = validateCertificate(request);

		// parse from complete JSON
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
		ResourceFromJsonVisitor visitor = new ResourceFromJsonVisitor();
		Resource resource = visitor.visit(jsonObject);

		AddResourceService svc = new AddResourceService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.rootElement = resource;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
			JsonElement jsonElement = resource.accept(toJsonVisitor);
			return Response.ok().entity(jsonElement.toString()).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Add a resource as JSON (flat)",
			description = "Adds a new resource to the system using flat JSON format.", responses = {
			@ApiResponse(responseCode = "200", description = "Resource added successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
			@ApiResponse(responseCode = "400", description = "Invalid JSON format."),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}")
	public Response addResourceAsJsonFlat(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @QueryParam("flat") boolean flat, String data) {

		Certificate cert = validateCertificate(request);

		Resource resource = parseNewResourceFromJson(cert, realm, type, data, flat);

		AddResourceService svc = new AddResourceService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.rootElement = resource;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
			if (flat)
				toJsonVisitor.flat();
			JsonElement jsonElement = resource.accept(toJsonVisitor);
			return Response.ok().entity(jsonElement.toString()).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Add an order as XML", description = "Adds a new order to the system using XML format.",
			requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			responses = {@ApiResponse(responseCode = "200", description = "Order added successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "400", description = "Invalid XML format."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/orders")
	public Response addOrderAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm, String data) {

		Certificate cert = validateCertificate(request);

		Order order = parseOrderFromXml(null, data);

		AddOrderService svc = new AddOrderService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.rootElement = order;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = order.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Add an order as JSON", description = "Adds a new order to the system using JSON format.",
			responses = {@ApiResponse(responseCode = "200", description = "Order added successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
					@ApiResponse(responseCode = "400", description = "Invalid JSON format."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders")
	public Response addOrderAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm, String data) {

		Certificate cert = validateCertificate(request);

		// parse from complete JSON
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
		OrderFromJsonVisitor visitor = new OrderFromJsonVisitor();
		Order order = visitor.visit(jsonObject);

		AddOrderService svc = new AddOrderService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.rootElement = order;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
			JsonElement jsonElement = order.accept(toJsonVisitor);
			return Response.ok().entity(jsonElement.toString()).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Add an order as JSON (flat)",
			description = "Adds a new order to the system using flat JSON format.", responses = {
			@ApiResponse(responseCode = "200", description = "Order added successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
			@ApiResponse(responseCode = "400", description = "Invalid JSON format."),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}")
	public Response addOrderAsJsonFlat(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @QueryParam("flat") boolean flat, String data) {

		Certificate cert = validateCertificate(request);

		Order order = parseNewOrderFromJson(cert, realm, type, data, flat);

		AddOrderService svc = new AddOrderService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.rootElement = order;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
			if (flat)
				toJsonVisitor.flat();
			JsonElement jsonElement = order.accept(toJsonVisitor);
			return Response.ok().entity(jsonElement.toString()).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Add an activity as XML", description = "Adds a new activity to the system using XML format.",
			requestBody = @io.swagger.v3.oas.annotations.parameters.RequestBody(
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
			responses = {@ApiResponse(responseCode = "200", description = "Activity added successfully.",
					content = @Content(mediaType = "application/xml",
							schema = @Schema(description = "See https://strolch.li/schema/StrolchModel.xsd"))),
					@ApiResponse(responseCode = "400", description = "Invalid XML format."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/activities")
	public Response addActivityAsXml(@Context HttpServletRequest request, @PathParam("realm") String realm,
			String data) {

		Certificate cert = validateCertificate(request);

		Activity activity = parseActivityFromXml(null, data);

		AddActivityService svc = new AddActivityService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.rootElement = activity;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = activity.accept(new StrolchElementToXmlStringVisitor());
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Add an activity as JSON",
			description = "Adds a new activity to the system using JSON format.", responses = {
			@ApiResponse(responseCode = "200", description = "Activity added successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
			@ApiResponse(responseCode = "400", description = "Invalid JSON format."),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities")
	public Response addActivityAsJson(@Context HttpServletRequest request, @PathParam("realm") String realm,
			String data) {

		Certificate cert = validateCertificate(request);

		// parse from complete JSON
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
		ActivityFromJsonVisitor visitor = new ActivityFromJsonVisitor();
		Activity activity = visitor.visit(jsonObject);

		AddActivityService svc = new AddActivityService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.rootElement = activity;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
			JsonElement jsonElement = activity.accept(toJsonVisitor);
			return Response.ok().entity(jsonElement.toString()).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Add an activity as JSON (flat)",
			description = "Adds a new activity to the system using flat JSON format.", responses = {
			@ApiResponse(responseCode = "200", description = "Activity added successfully.",
					content = @Content(mediaType = "application/json", schema = @Schema(type = "object"))),
			@ApiResponse(responseCode = "400", description = "Invalid JSON format."),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@POST
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}")
	public Response addActivityAsJsonFlat(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @QueryParam("flat") boolean flat, String data) {

		Certificate cert = validateCertificate(request);

		Activity activity = parseNewActivityFromJson(cert, realm, type, data, flat);

		AddActivityService svc = new AddActivityService();
		StrolchRootElementArgument arg = new StrolchRootElementArgument();
		arg.rootElement = activity;
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			StrolchElementToJsonVisitor toJsonVisitor = new StrolchElementToJsonVisitor().withLocator().withVersion();
			if (flat)
				toJsonVisitor.flat();
			JsonElement jsonElement = activity.accept(toJsonVisitor);
			return Response.ok().entity(jsonElement.toString()).build();
		}

		return toResponse(result);
	}

	@Operation(summary = "Remove resources by type",
			description = "Removes multiple resources of a specified type within a realm.", responses = {
			@ApiResponse(responseCode = "200", description = "Resources removed successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "400", description = "Invalid request format."),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}")
	public Response removeResourcesByType(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @QueryParam("ids") String ids) {

		Certificate cert = validateCertificate(request);

		RemoveResourcesService svc = new RemoveResourcesService();
		LocatorListArgument arg = svc.getArgumentInstance();
		arg.locators = new ArrayList<>();
		arg.realm = realm;

		for (String id : ids.split(",")) {
			Locator locator = Resource.locatorFor(type, decodeId(id.trim()).trim());
			arg.locators.add(locator);
		}

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		return toResponse(result);
	}

	@Operation(summary = "Remove orders by type",
			description = "Removes multiple orders of a specified type within a realm.", responses = {
			@ApiResponse(responseCode = "200", description = "Orders removed successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "400", description = "Invalid request format."),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}")
	public Response removeOrdersByType(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @QueryParam("ids") String ids) {

		Certificate cert = validateCertificate(request);

		RemoveOrdersService svc = new RemoveOrdersService();
		LocatorListArgument arg = svc.getArgumentInstance();
		arg.locators = new ArrayList<>();
		arg.realm = realm;

		for (String id : ids.split(",")) {
			Locator locator = Order.locatorFor(type, decodeId(id.trim()));
			arg.locators.add(locator);
		}

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		return toResponse(result);
	}

	@Operation(summary = "Remove activities by type",
			description = "Removes multiple activities of a specified type within a realm.", responses = {
			@ApiResponse(responseCode = "200", description = "Activities removed successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
			@ApiResponse(responseCode = "400", description = "Invalid request format."),
			@ApiResponse(responseCode = "403", description = "Access denied."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}")
	public Response removeActivitiesByType(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@PathParam("type") String type, @QueryParam("ids") String ids) {

		Certificate cert = validateCertificate(request);

		RemoveActivitiesService svc = new RemoveActivitiesService();
		LocatorListArgument arg = svc.getArgumentInstance();
		arg.locators = new ArrayList<>();
		arg.realm = realm;

		for (String id : ids.split(",")) {
			Locator locator = Activity.locatorFor(type, decodeId(id.trim()));
			arg.locators.add(locator);
		}

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		return toResponse(result);
	}

	@Operation(summary = "Remove a specific resource", description = "Removes a single resource from the system by ID.",
			responses = {@ApiResponse(responseCode = "200", description = "Resource removed successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
					@ApiResponse(responseCode = "404", description = "Resource not found."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resources/{type}/{id}")
	public Response removeResource(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		RemoveResourceService svc = new RemoveResourceService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.locator = Resource.locatorFor(type, id);
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		return toResponse(result);
	}

	@Operation(summary = "Remove a specific order", description = "Removes a single order from the system by ID.",
			responses = {@ApiResponse(responseCode = "200", description = "Order removed successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
					@ApiResponse(responseCode = "404", description = "Order not found."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/orders/{type}/{id}")
	public Response removeOrder(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		RemoveOrderService svc = new RemoveOrderService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.locator = Order.locatorFor(type, id);
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		return toResponse(result);
	}

	@Operation(summary = "Remove a specific activity", description = "Removes a single activity from the system by ID.",
			responses = {@ApiResponse(responseCode = "200", description = "Activity removed successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = ServiceResultResponse.class))),
					@ApiResponse(responseCode = "404", description = "Activity not found."),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@DELETE
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/activities/{type}/{id}")
	public Response removeActivity(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {

		Certificate cert = validateCertificate(request);
		id = decodeId(id);

		RemoveActivityService svc = new RemoveActivityService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.locator = Activity.locatorFor(type, id);
		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(cert, svc, arg);
		return toResponse(result);
	}

	private Resource parseResourceFromXml(String type, String data) {
		Resource resource;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			getSaxParser().parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getResources().isEmpty())
				throw new StrolchPersistenceException("No Resource parsed from xml value" + (
						StringHelper.isNotEmpty(type) ? " for type " + type : ""));
			if (listener.getResources().size() > 1)
				throw new StrolchPersistenceException("Multiple Resources parsed from xml value" + (
						StringHelper.isNotEmpty(type) ? " for type " + type : ""));

			resource = listener.getResources().getFirst();
			resource.setVersion(null);

			DBC.INTERIM.assertEquals("Posted type must be same as request!", type, resource.getType());

		} catch (Exception e) {
			throw new StrolchPersistenceException("Failed to extract Resource from xml value" + (
					StringHelper.isNotEmpty(type) ? " for type " + type : ""), e);
		}
		return resource;
	}

	private Order parseOrderFromXml(String type, String data) {
		Order order;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			getSaxParser().parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getOrders().isEmpty())
				throw new StrolchPersistenceException(
						"No Order parsed from xml value" + (StringHelper.isNotEmpty(type) ? " for type " + type : ""));
			if (listener.getOrders().size() > 1)
				throw new StrolchPersistenceException("Multiple Orders parsed from xml value" + (
						StringHelper.isNotEmpty(type) ? " for type " + type : ""));

			order = listener.getOrders().getFirst();
			order.setVersion(null);

			DBC.INTERIM.assertEquals("Posted type must be same as request!", type, order.getType());

		} catch (Exception e) {
			throw new StrolchPersistenceException("Failed to extract Order from xml value" + (
					StringHelper.isNotEmpty(type) ? " for type " + type : ""), e);
		}
		return order;
	}

	private Activity parseActivityFromXml(String type, String data) {
		Activity activity;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			getSaxParser().parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getActivities().isEmpty())
				throw new StrolchPersistenceException("No Activity parsed from xml value" + (
						StringHelper.isNotEmpty(type) ? " for type " + type : ""));
			if (listener.getActivities().size() > 1)
				throw new StrolchPersistenceException("Multiple Activities parsed from xml value" + (
						StringHelper.isNotEmpty(type) ? " for type " + type : ""));

			activity = listener.getActivities().getFirst();
			activity.setVersion(null);

			DBC.INTERIM.assertEquals("Posted type must be same as request!", type, activity.getType());

		} catch (Exception e) {
			throw new StrolchPersistenceException("Failed to extract Activity from xml value" + (
					StringHelper.isNotEmpty(type) ? " for type " + type : ""), e);
		}
		return activity;
	}

	private Resource parseNewResourceFromJson(Certificate cert, String realm, String type, String data, boolean flat) {

		// parse JSON string
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
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
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
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
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
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
