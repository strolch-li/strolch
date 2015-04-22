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

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.xml.XmlKeyValue;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/privilege/policies")
public class PrivilegePoliciesService {

	// private static final Logger logger = LoggerFactory.getLogger(PrivilegePoliciesService.class);

	private PrivilegeHandler getPrivilegeHandler(Certificate cert) {
		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		return container.getPrivilegeHandler().getPrivilegeHandler(cert);
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getRoles(@Context HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = getPrivilegeHandler(cert);

		Map<String, String> policyDefs = privilegeHandler.getPolicyDefs(cert);
		List<XmlKeyValue> values = XmlKeyValue.valueOf(policyDefs);
		GenericEntity<List<XmlKeyValue>> entity = new GenericEntity<List<XmlKeyValue>>(values) {
		};
		return Response.ok(entity, MediaType.APPLICATION_JSON).build();
	}
}
