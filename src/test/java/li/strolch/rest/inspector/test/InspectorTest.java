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

import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.core.MediaType;

import li.strolch.rest.inspector.model.AgentOverview;
import li.strolch.rest.inspector.model.RealmOverview;

import org.eclipse.persistence.jaxb.rs.MOXyJsonProvider;
import org.junit.Test;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.GenericType;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InspectorTest extends AbstractRestfulTest {

	protected WebResource getResource() {
		ClientConfig cc = new DefaultClientConfig();
		cc.getClasses().add(MOXyJsonProvider.class);
		Client client = Client.create(cc);
		WebResource resource = client.resource(BASE_URI);
		return resource;
	}

	protected ClientResponse getClientResponse(String path) {
		WebResource resource = getResource();
		ClientResponse response = resource.path(path).accept(MediaType.APPLICATION_JSON_TYPE).get(ClientResponse.class);
		return response;
	}

	@Test
	public void shouldGetAgent() {

		// expected result
		List<RealmOverview> realms = new ArrayList<>(1);
		realms.add(new RealmOverview("defaultRealm", 4));
		AgentOverview expectedAgentOverview = new AgentOverview(realms);

		// query
		ClientResponse response = getClientResponse("/strolch/inspector");
		AgentOverview agentOverview = response.getEntity(new GenericType<AgentOverview>() {
		});

		// assertions
		assertEquals(expectedAgentOverview, agentOverview);
	}
}
