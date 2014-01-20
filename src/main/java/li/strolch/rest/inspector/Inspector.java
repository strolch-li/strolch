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
package li.strolch.rest.inspector;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.impl.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.rest.inspector.model.AgentOverview;
import li.strolch.rest.inspector.model.ElementMapOverview;
import li.strolch.rest.inspector.model.ElementMapType;
import li.strolch.rest.inspector.model.ElementMapsOverview;
import li.strolch.rest.inspector.model.OrderOverview;
import li.strolch.rest.inspector.model.RealmDetail;
import li.strolch.rest.inspector.model.RealmOverview;
import li.strolch.rest.inspector.model.ResourceOverview;
import li.strolch.rest.inspector.model.StrolchElementOverview;
import li.strolch.rest.inspector.model.TypeDetail;
import li.strolch.rest.inspector.model.TypeOverview;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/inspector")
public class Inspector {

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
	public Response getAgent() {

		try {
			ComponentContainer container = AgentRef.getInstance().getContainer();
			Set<String> realmNames = container.getRealmNames();
			List<RealmOverview> realmOverviews = new ArrayList<>(realmNames.size());
			for (String realmName : realmNames) {

				StrolchRealm realm = container.getRealm(realmName);
				try (StrolchTransaction tx = realm.openTx()) {
					long size = 0;
					size += realm.getResourceMap().querySize(tx);
					size += realm.getOrderMap().querySize(tx);
					RealmOverview realmOverview = new RealmOverview(realmName, size);
					realmOverviews.add(realmOverview);
				}
			}

			AgentOverview agentOverview = new AgentOverview(realmOverviews);
			GenericEntity<AgentOverview> entity = new GenericEntity<AgentOverview>(agentOverview, AgentOverview.class) {
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
	public Response getRealm(@PathParam("realm") String realm) {
		DBC.PRE.assertNotEmpty("Realm must be set!", realm);

		StrolchRealm strolchRealm = AgentRef.getInstance().getContainer().getRealm(realm);
		List<ElementMapsOverview> elementMapOverviews = new ArrayList<>(2);
		try (StrolchTransaction tx = strolchRealm.openTx()) {

			ResourceMap resourceMap = strolchRealm.getResourceMap();
			ElementMapsOverview resourceOverview = new ElementMapsOverview(ElementMapType.RESOURCE);
			resourceOverview.setNrOfElements(resourceMap.querySize(tx));
			resourceOverview.setTypes(resourceMap.getTypes(tx));
			elementMapOverviews.add(resourceOverview);

			OrderMap orderMap = strolchRealm.getOrderMap();
			ElementMapsOverview orderOverview = new ElementMapsOverview(ElementMapType.ORDER);
			orderOverview.setNrOfElements(orderMap.querySize(tx));
			orderOverview.setTypes(orderMap.getTypes(tx));
			elementMapOverviews.add(orderOverview);
		}

		RealmDetail modelOverview = new RealmDetail(elementMapOverviews);
		GenericEntity<RealmDetail> entity = new GenericEntity<RealmDetail>(modelOverview, RealmDetail.class) {
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
	@Path("{realm}/resource")
	public Response getResourcesOverview(@PathParam("realm") String realm) {
		DBC.PRE.assertNotEmpty("Realm must be set!", realm);
		StrolchRealm strolchRealm = AgentRef.getInstance().getContainer().getRealm(realm);

		ElementMapOverview resourcesOverview;
		try (StrolchTransaction tx = strolchRealm.openTx()) {
			ResourceMap resourceMap = tx.getResourceMap();
			Set<String> types = resourceMap.getTypes(tx);
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
		};
		return Response.ok().entity(entity).build();
	}

	/**
	 * <p>
	 * Order inspector
	 * </p>
	 * <p>
	 * Returns an overview of the {@link Order Orderss}. This is a list of all the types and the size each type has
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
	@Path("{realm}/order")
	public Response getOrdersOverview(@PathParam("realm") String realm) {
		DBC.PRE.assertNotEmpty("Realm must be set!", realm);
		StrolchRealm strolchRealm = AgentRef.getInstance().getContainer().getRealm(realm);

		ElementMapOverview ordersOverview;
		try (StrolchTransaction tx = strolchRealm.openTx()) {
			OrderMap orderMap = tx.getOrderMap();
			Set<String> types = orderMap.getTypes(tx);
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
		};
		return Response.ok().entity(entity).build();
	}

	// TODO for the get element type details, we should not simply query all objects, but rather find a solution to query only the id, name, type and date, state for the order

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resource/{type}")
	public Response getResourceTypeDetails(@PathParam("realm") String realm, @PathParam("type") String type) {
		DBC.PRE.assertNotEmpty("Realm must be set!", realm);
		StrolchRealm strolchRealm = AgentRef.getInstance().getContainer().getRealm(realm);

		TypeDetail typeDetail;
		try (StrolchTransaction tx = strolchRealm.openTx()) {
			List<Resource> byType = tx.getResourceMap().getElementsBy(tx, type);
			List<StrolchElementOverview> elementOverviews = new ArrayList<>(byType.size());
			for (Resource resource : byType) {
				ResourceOverview resourceOverview = new ResourceOverview(resource.getId(), resource.getName(),
						resource.getType());
				elementOverviews.add(resourceOverview);
			}
			typeDetail = new TypeDetail(type, elementOverviews);
		}

		GenericEntity<TypeDetail> entity = new GenericEntity<TypeDetail>(typeDetail, TypeDetail.class) {
		};
		return Response.ok().entity(entity).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/order/{type}")
	public Response getOrderTypeDetails(@PathParam("realm") String realm, @PathParam("type") String type) {
		DBC.PRE.assertNotEmpty("Realm must be set!", realm);
		StrolchRealm strolchRealm = AgentRef.getInstance().getContainer().getRealm(realm);

		TypeDetail typeDetail;
		try (StrolchTransaction tx = strolchRealm.openTx()) {
			List<Order> byType = tx.getOrderMap().getElementsBy(tx, type);
			List<StrolchElementOverview> elementOverviews = new ArrayList<>(byType.size());
			for (Order order : byType) {
				OrderOverview orderOverview = new OrderOverview(order.getId(), order.getName(), order.getType(),
						order.getDate(), order.getState());
				elementOverviews.add(orderOverview);
			}
			typeDetail = new TypeDetail(type, elementOverviews);
		}

		GenericEntity<TypeDetail> entity = new GenericEntity<TypeDetail>(typeDetail, TypeDetail.class) {
		};
		return Response.ok().entity(entity).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/resource/{type}/{id}")
	public Response getResource(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id) {
		DBC.PRE.assertNotEmpty("Realm must be set!", realm);
		StrolchRealm strolchRealm = AgentRef.getInstance().getContainer().getRealm(realm);

		Resource resource;
		try (StrolchTransaction tx = strolchRealm.openTx()) {
			resource = tx.getResourceMap().getBy(tx, type, id);
		}
		if (resource == null) {
			throw new StrolchException("No Resource exists for " + type + "/" + id);
		}

		ResourceOverview resourceOverview = new ResourceOverview(resource.getId(), resource.getName(),
				resource.getType());
		GenericEntity<ResourceOverview> entity = new GenericEntity<ResourceOverview>(resourceOverview,
				ResourceOverview.class) {
		};
		return Response.ok().entity(entity).build();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{realm}/order/{type}/{id}")
	public Response getOrder(@PathParam("realm") String realm, @PathParam("type") String type,
			@PathParam("id") String id) {
		DBC.PRE.assertNotEmpty("Realm must be set!", realm);
		StrolchRealm strolchRealm = AgentRef.getInstance().getContainer().getRealm(realm);

		Order order;
		try (StrolchTransaction tx = strolchRealm.openTx()) {
			order = tx.getOrderMap().getBy(tx, type, id);
		}
		if (order == null) {
			throw new StrolchException("No Order exists for " + type + "/" + id);
		}

		OrderOverview orderOverview = new OrderOverview(order.getId(), order.getName(), order.getType(),
				order.getDate(), order.getState());
		GenericEntity<OrderOverview> entity = new GenericEntity<OrderOverview>(orderOverview, OrderOverview.class) {
		};
		return Response.ok().entity(entity).build();
	}
}
