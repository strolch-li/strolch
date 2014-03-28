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
package li.strolch.rest.inspector.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import li.strolch.rest.model.AgentOverview;
import li.strolch.rest.model.ElementMapOverview;
import li.strolch.rest.model.ElementMapType;
import li.strolch.rest.model.ElementMapsOverview;
import li.strolch.rest.model.RealmDetail;
import li.strolch.rest.model.RealmOverview;
import li.strolch.rest.model.TypeOverview;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InspectorTest extends AbstractRestfulTest {

	private static final String ROOT_PATH = "strolch/inspector/";

	@Test
	public void shouldGetAgent() {

		// expected result
		List<RealmOverview> realms = new ArrayList<>(1);
		realms.add(new RealmOverview("defaultRealm", 4));
		AgentOverview expectedAgentOverview = new AgentOverview(realms);

		// query
		Response result = target().path(ROOT_PATH).request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		AgentOverview agentOverview = result.readEntity(AgentOverview.class);

		// assertions
		assertEquals(expectedAgentOverview, agentOverview);
	}

	@Test
	public void shouldGetRealm() {

		// expected result
		List<ElementMapsOverview> elementMapOverviews = new ArrayList<>(2);
		Set<String> resourceTypes = new HashSet<>();
		resourceTypes.add("Template");
		resourceTypes.add("TestType");
		elementMapOverviews.add(new ElementMapsOverview(ElementMapType.RESOURCE, 2, resourceTypes));
		Set<String> orderTypes = new HashSet<>();
		orderTypes.add("Template");
		orderTypes.add("TestType");
		elementMapOverviews.add(new ElementMapsOverview(ElementMapType.ORDER, 2, orderTypes));
		RealmDetail expectedRealmDetail = new RealmDetail(elementMapOverviews);

		// query
		Response result = target().path(ROOT_PATH + "defaultRealm").request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		RealmDetail realmDetail = result.readEntity(RealmDetail.class);

		// assertions
		assertEquals(expectedRealmDetail, realmDetail);
	}

	@Test
	public void shouldGetResourcesOverview() {

		// expected result
		String elementMapName = "Resource";
		List<TypeOverview> typeOverviews = new ArrayList<>(2);
		typeOverviews.add(new TypeOverview("Template", 1));
		typeOverviews.add(new TypeOverview("TestType", 1));
		ElementMapOverview expectedElementMapOverview = new ElementMapOverview(elementMapName, typeOverviews);

		// query
		Response result = target().path(ROOT_PATH + "defaultRealm/resource").request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		ElementMapOverview elementMapOverview = result.readEntity(ElementMapOverview.class);

		// assertions
		assertEquals(expectedElementMapOverview, elementMapOverview);
	}

	@Test
	public void shouldGetOrdersOverview() {

		// expected result
		String elementMapName = "Order";
		List<TypeOverview> typeOverviews = new ArrayList<>(2);
		typeOverviews.add(new TypeOverview("Template", 1));
		typeOverviews.add(new TypeOverview("TestType", 1));
		ElementMapOverview expectedElementMapOverview = new ElementMapOverview(elementMapName, typeOverviews);

		// query
		Response result = target().path(ROOT_PATH + "defaultRealm/order").request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		ElementMapOverview elementMapOverview = result.readEntity(ElementMapOverview.class);

		// assertions
		assertEquals(expectedElementMapOverview, elementMapOverview);
	}

	// TODO modify object model to include discriminator values, so that we can parse the objects

	@Test
	public void shouldGetResourceTypeDetails() {

		// query
		Response result = target().path(ROOT_PATH + "defaultRealm/resource/Template")
				.request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		String entity = result.readEntity(String.class);
		String expected = "{\"type\":\"Template\",\"resources\":[{\"id\":\"TestType\",\"name\":\"TestType Template\",\"type\":\"Template\"}]}";
		assertEquals(expected, entity);
	}

	@Test
	public void shouldGetOrderTypeDetails() {

		// query
		Response result = target().path(ROOT_PATH + "defaultRealm/order/Template").request(MediaType.APPLICATION_JSON)
				.get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		String entity = result.readEntity(String.class);
		String expected = "{\"type\":\"Template\",\"orders\":[{\"id\":\"TestType\",\"name\":\"MyTestOrder Template\",\"type\":\"Template\",\"date\":\"2012-11-30T18:12:05.628+01:00\",\"state\":\"CREATED\"}]}";
		assertEquals(expected, entity);
	}

	@Test
	public void shouldGetResource() {

		// query
		Response result = target().path(ROOT_PATH + "defaultRealm/resource/Template/TestType")
				.request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		String entity = result.readEntity(String.class);
		assertTrue(entity.contains("name\":\"TestType Template\",\"type\":\"Template\",\"parameterBags\":"));
	}

	@Test
	public void shouldGetOrder() {

		// query
		Response result = target().path(ROOT_PATH + "defaultRealm/order/Template/TestType")
				.request(MediaType.APPLICATION_JSON).get();
		assertEquals(Status.OK.getStatusCode(), result.getStatus());
		String entity = result.readEntity(String.class);
		assertTrue(entity
				.contains("\"date\":\"2012-11-30T18:12:05.628+01:00\",\"state\":\"CREATED\",\"parameterBags\""));
	}
}
