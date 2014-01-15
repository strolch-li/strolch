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

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.impl.StrolchRealm;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.rest.inspector.model.ElementMapOverview;
import li.strolch.rest.inspector.model.ElementMapType;
import li.strolch.rest.inspector.model.ModelOverview;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/inspector")
public class Inspector {

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getRoot(@QueryParam("realm") String realm) {
		DBC.PRE.assertNotEmpty("Realm must be set!", realm);

		StrolchRealm strolchRealm = AgentRef.getInstance().getContainer().getRealm(realm);
		ModelOverview modelOverview = new ModelOverview();

		try (StrolchTransaction tx = strolchRealm.openTx()) {

			ResourceMap resourceMap = strolchRealm.getResourceMap();
			ElementMapOverview resourceOverview = new ElementMapOverview(ElementMapType.RESOURCE);
			resourceOverview.setNrOfElements(resourceMap.querySize(tx));
			resourceOverview.setTypes(resourceMap.getTypes(tx));
			modelOverview.getElementMapOverviews().add(resourceOverview);

			OrderMap orderMap = strolchRealm.getOrderMap();
			ElementMapOverview orderOverview = new ElementMapOverview(ElementMapType.ORDER);
			orderOverview.setNrOfElements(orderMap.querySize(tx));
			orderOverview.setTypes(orderMap.getTypes(tx));
			modelOverview.getElementMapOverviews().add(orderOverview);
		}

		GenericEntity<ModelOverview> entity = new GenericEntity<ModelOverview>(modelOverview, ModelOverview.class) {
		};
		return Response.ok().entity(entity).build();
	}
}
