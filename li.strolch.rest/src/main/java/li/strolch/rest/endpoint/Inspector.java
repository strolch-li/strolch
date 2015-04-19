/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.xml.OrderToXmlStringVisitor;
import li.strolch.model.xml.ResourceToXmlStringVisitor;
import li.strolch.model.xml.SimpleStrolchElementListener;
import li.strolch.model.xml.XmlModelSaxReader;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.model.AgentOverview;
import li.strolch.rest.model.ElementMapOverview;
import li.strolch.rest.model.ElementMapType;
import li.strolch.rest.model.ElementMapsOverview;
import li.strolch.rest.model.OrderDetail;
import li.strolch.rest.model.OrderOverview;
import li.strolch.rest.model.RealmDetail;
import li.strolch.rest.model.RealmOverview;
import li.strolch.rest.model.ResourceDetail;
import li.strolch.rest.model.ResourceOverview;
import li.strolch.rest.model.Result;
import li.strolch.rest.model.StrolchElementOverview;
import li.strolch.rest.model.TypeDetail;
import li.strolch.rest.model.TypeOverview;
import li.strolch.service.UpdateOrderService;
import li.strolch.service.UpdateOrderService.UpdateOrderArg;
import li.strolch.service.UpdateResourceService;
import li.strolch.service.UpdateResourceService.UpdateResourceArg;
import li.strolch.service.api.ServiceResult;

import org.xml.sax.InputSource;

import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/inspector")
public class Inspector {

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().getContainer().getRealm(realm)
				.openTx(certificate, Inspector.class);
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
	 * 
	 * @see AgentOverview
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAgent(@Context HttpServletRequest request) {
		try {
			ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
			Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

			Set<String> realmNames = container.getRealmNames();
			List<RealmOverview> realmOverviews = new ArrayList<>(realmNames.size());
			for (String realmName : realmNames) {

				try (StrolchTransaction tx = openTx(cert, realmName)) {
					long size = 0;
					size += tx.getResourceMap().querySize(tx);
					size += tx.getOrderMap().querySize(tx);
					RealmOverview realmOverview = new RealmOverview(realmName, size);
					realmOverviews.add(realmOverview);
				}
			}

			AgentOverview agentOverview = new AgentOverview(realmOverviews);
			GenericEntity<AgentOverview> entity = new GenericEntity<AgentOverview>(agentOverview, AgentOverview.class) {
				//
			};
			return Response.ok().entity(entity).build();
		} catch (Exception e) {
			//e.printStackTrace();
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
	 * 
	 * @see RealmDetail
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}")
	public Response getRealm(@PathParam("realm") String realm, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		List<ElementMapsOverview> elementMapOverviews = new ArrayList<>(2);
		try (StrolchTransaction tx = openTx(cert, realm)) {

			ResourceMap resourceMap = tx.getResourceMap();
			ElementMapsOverview resourceOverview = new ElementMapsOverview(ElementMapType.RESOURCE);
			resourceOverview.setNrOfElements(resourceMap.querySize(tx));
			resourceOverview.setTypes(resourceMap.getTypes(tx));
			elementMapOverviews.add(resourceOverview);

			OrderMap orderMap = tx.getOrderMap();
			ElementMapsOverview orderOverview = new ElementMapsOverview(ElementMapType.ORDER);
			orderOverview.setNrOfElements(orderMap.querySize(tx));
			orderOverview.setTypes(orderMap.getTypes(tx));
			elementMapOverviews.add(orderOverview);
		}

		RealmDetail modelOverview = new RealmDetail(elementMapOverviews);
		GenericEntity<RealmDetail> entity = new GenericEntity<RealmDetail>(modelOverview, RealmDetail.class) {
			//
		};
		return Response.ok().entity(entity).build();
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
	 * 
	 * @see ElementMapOverview
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/Resource")
	public Response getResourcesOverview(@PathParam("realm") String realm, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ElementMapOverview resourcesOverview;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			ResourceMap resourceMap = tx.getResourceMap();
			List<String> types = new ArrayList<>(resourceMap.getTypes(tx));
			Collections.sort(types);
			List<TypeOverview> typeOverviews = new ArrayList<>(types.size());
			for (String type : types) {
				long size = resourceMap.querySize(tx, type);
				TypeOverview typeOverview = new TypeOverview(type, size);
				typeOverviews.add(typeOverview);
			}

			resourcesOverview = new ElementMapOverview(ElementMapType.RESOURCE.getName(), typeOverviews);
		}

		GenericEntity<ElementMapOverview> entity = new GenericEntity<ElementMapOverview>(resourcesOverview,
				ElementMapOverview.class) {
			//
		};
		return Response.ok().entity(entity).build();
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
	 * 
	 * @see ElementMapOverview
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/Order")
	public Response getOrdersOverview(@PathParam("realm") String realm, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		ElementMapOverview ordersOverview;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			OrderMap orderMap = tx.getOrderMap();
			List<String> types = new ArrayList<>(orderMap.getTypes(tx));
			Collections.sort(types);
			List<TypeOverview> typeOverviews = new ArrayList<>(types.size());
			for (String type : types) {
				long size = orderMap.querySize(tx, type);
				TypeOverview typeOverview = new TypeOverview(type, size);
				typeOverviews.add(typeOverview);
			}

			ordersOverview = new ElementMapOverview(ElementMapType.ORDER.getName(), typeOverviews);
		}

		GenericEntity<ElementMapOverview> entity = new GenericEntity<ElementMapOverview>(ordersOverview,
				ElementMapOverview.class) {
			//
		};
		return Response.ok().entity(entity).build();
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
	 * 
	 * @return an overview of the {@link Resource Resources} with the given type. This is a list of overviews of the
	 *         resources
	 * 
	 * @see TypeDetail
	 * @see StrolchElementOverview
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/Resource/{type}")
	public Response getResourceTypeDetails(@PathParam("realm") String realm, @PathParam("type") String type,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		TypeDetail typeDetail;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			List<Resource> byType = tx.getResourceMap().getElementsBy(tx, type);
			List<StrolchElementOverview> elementOverviews = new ArrayList<>(byType.size());
			for (Resource resource : byType) {
				ResourceOverview resourceOverview = new ResourceOverview(resource);
				elementOverviews.add(resourceOverview);
			}
			typeDetail = new TypeDetail(type, elementOverviews);
		}

		GenericEntity<TypeDetail> entity = new GenericEntity<TypeDetail>(typeDetail, TypeDetail.class) {
			//
		};
		return Response.ok().entity(entity).build();
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
	 * 
	 * @see TypeDetail
	 * @see StrolchElementOverview
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/Order/{type}")
	public Response getOrderTypeDetails(@PathParam("realm") String realm, @PathParam("type") String type,
			@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		TypeDetail typeDetail;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			List<Order> byType = tx.getOrderMap().getElementsBy(tx, type);
			List<StrolchElementOverview> elementOverviews = new ArrayList<>(byType.size());
			for (Order order : byType) {
				OrderOverview orderOverview = new OrderOverview(order);
				elementOverviews.add(orderOverview);
			}
			typeDetail = new TypeDetail(type, elementOverviews);
		}

		GenericEntity<TypeDetail> entity = new GenericEntity<TypeDetail>(typeDetail, TypeDetail.class) {
			//
		};
		return Response.ok().entity(entity).build();
	}

	/**
	 * <p>
	 * Resource inspector
	 * </p>
	 * 
	 * <p>
	 * Returns the resource with the given id
	 * </p>
	 * 
	 * @param realm
	 *            the realm for which the resource is to be returned
	 * @param type
	 *            the type of the resource
	 * @param id
	 *            the id of the resource
	 * 
	 * @return the resource with the given id
	 * 
	 * @see ResourceDetail
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/Resource/{type}/{id}")
	public Response getResourceAsJson(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Resource resource;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			resource = tx.getResourceMap().getBy(tx, type, id);
		}
		if (resource == null) {
			throw new StrolchException(MessageFormat.format("No Resource exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		ResourceDetail resourceDetail = new ResourceDetail(resource);
		GenericEntity<ResourceDetail> entity = new GenericEntity<ResourceDetail>(resourceDetail, ResourceDetail.class) {
			//
		};
		return Response.ok().entity(entity).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/Resource/{type}/{id}")
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

		String asXml = new ResourceToXmlStringVisitor().visit(resource);
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/Resource/{type}/{id}")
	public Response updateResourceAsXml(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, String data, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Resource resource;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getResources().size() == 0)
				throw new StrolchPersistenceException(MessageFormat.format(
						"No Resources parsed from xml value for {0} / {1}", id, type));
			if (listener.getResources().size() > 1)
				throw new StrolchPersistenceException(MessageFormat.format(
						"Multiple Resources parsed from xml value for {0} / {1}", id, type));

			resource = listener.getResources().get(0);

		} catch (Exception e) {
			throw new StrolchPersistenceException(MessageFormat.format(
					"Failed to extract Resources from xml value for {0} / {1}", id, type), e);
		}

		UpdateResourceService svc = new UpdateResourceService();
		UpdateResourceArg arg = new UpdateResourceArg();
		arg.resource = resource;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = new ResourceToXmlStringVisitor().visit(resource);
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return Result.toResponse(result);
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/Order/{type}/{id}")
	public Response getOrderAsJson(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Order order;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			order = tx.getOrderMap().getBy(tx, type, id);
		}
		if (order == null) {
			throw new StrolchException(MessageFormat.format("No Order exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		OrderDetail orderDetail = new OrderDetail(order);
		GenericEntity<OrderDetail> entity = new GenericEntity<OrderDetail>(orderDetail, OrderDetail.class) {
			//
		};
		return Response.ok().entity(entity).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_XML)
	@Path("{realm}/Order/{type}/{id}")
	public Response getOrderAsXml(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Order order;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			order = tx.getOrderMap().getBy(tx, type, id);
		}
		if (order == null) {
			throw new StrolchException(MessageFormat.format("No Order exists for {0}/{1}", type, id)); //$NON-NLS-1$
		}

		String asXml = new OrderToXmlStringVisitor().visit(order);
		return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
	}

	@PUT
	@Produces(MediaType.APPLICATION_XML)
	@Consumes(MediaType.APPLICATION_XML)
	@Path("{realm}/Order/{type}/{id}")
	public Response updateOrderAsXml(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id, String data, @Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Order order;
		try {
			SimpleStrolchElementListener listener = new SimpleStrolchElementListener();
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			parser.parse(new InputSource(new StringReader(data)), new XmlModelSaxReader(listener));

			if (listener.getOrders().size() == 0)
				throw new StrolchPersistenceException(MessageFormat.format(
						"No Orders parsed from xml value for {0} / {1}", id, type));
			if (listener.getOrders().size() > 1)
				throw new StrolchPersistenceException(MessageFormat.format(
						"Multiple Orders parsed from xml value for {0} / {1}", id, type));

			order = listener.getOrders().get(0);

		} catch (Exception e) {
			throw new StrolchPersistenceException(MessageFormat.format(
					"Failed to extract Order from xml value for {0} / {1}", id, type), e);
		}

		UpdateOrderService svc = new UpdateOrderService();
		UpdateOrderArg arg = new UpdateOrderArg();
		arg.order = order;
		arg.realm = realm;

		ServiceResult result = RestfulStrolchComponent.getInstance().getServiceHandler().doService(cert, svc, arg);
		if (result.isOk()) {
			String asXml = new OrderToXmlStringVisitor().visit(order);
			return Response.ok().type(MediaType.APPLICATION_XML).entity(asXml).build();
		}

		return Result.toResponse(result);
	}
}
