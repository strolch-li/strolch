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

import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toList;
import static li.strolch.model.StrolchModelConstants.ROLE_STROLCH_ADMIN;
import static li.strolch.rest.StrolchRestfulConstants.DATA;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.job.StrolchJob;
import li.strolch.job.StrolchJobsHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/jobs")
public class StrolchJobsResource {

	private static final Logger logger = LoggerFactory.getLogger(StrolchJobsResource.class);

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[1];
		return element.getClassName() + "." + element.getMethodName();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAll(@Context HttpServletRequest request, @Context HttpHeaders headers) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		String source = (String) request.getAttribute(StrolchRestfulConstants.STROLCH_REQUEST_SOURCE);
		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();

		try (StrolchTransaction tx = RestfulStrolchComponent.getInstance().openTx(cert, getContext())) {

			// assert user can access StrolchJobs
			PrivilegeContext ctx = tx.getPrivilegeContext();
			if (!ctx.hasRole(ROLE_STROLCH_ADMIN))
				ctx.assertHasPrivilege(StrolchJob.class.getName());

			StrolchJobsHandler strolchJobsHandler = container.getComponent(StrolchJobsHandler.class);

			List<StrolchJob> jobs = strolchJobsHandler.getJobs(cert, source).stream() //
					.filter(job -> {
						if (ctx.hasRole(ROLE_STROLCH_ADMIN))
							return true;

						IPrivilege privilege = ctx.getPrivilege(StrolchJob.class.getName());
						return privilege.isAllAllowed() || privilege.getAllowList().contains(job.getClass().getName());
					}) //
					.sorted(comparing(StrolchJob::getName)) //
					.collect(toList());
			return ResponseUtil.listToResponse(DATA, jobs, StrolchJob::toJson);
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{name}/action")
	public Response doAction(@Context HttpServletRequest request, @Context HttpHeaders headers,
			@PathParam("name") String name, @QueryParam("action") String action) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		String source = (String) request.getAttribute(StrolchRestfulConstants.STROLCH_REQUEST_SOURCE);

		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		StrolchJobsHandler strolchJobsHandler = container.getComponent(StrolchJobsHandler.class);

		try {
			PrivilegeContext ctx = container.getPrivilegeHandler().validate(cert);
			StrolchJob job = strolchJobsHandler.getJob(cert, source, name);

			// assert user can access StrolchJobs
			if (!ctx.hasRole(ROLE_STROLCH_ADMIN))
				ctx.validateAction(job);

			switch (action) {
			case "runNow" -> job.runNow();
			case "schedule" -> job.schedule();
			case "cancel" -> job.cancel(true);
			default -> throw new IllegalArgumentException("Unhandled action " + action);
			}

			return ResponseUtil.toResponse();

		} catch (Exception e) {
			logger.error("Failed to perform job action " + action, e);
			return ResponseUtil.toResponse(e);
		}
	}
}
