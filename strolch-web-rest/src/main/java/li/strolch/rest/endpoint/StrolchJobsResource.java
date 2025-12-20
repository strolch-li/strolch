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

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.job.StrolchJob;
import li.strolch.job.StrolchJobsHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.SimpleRestrictable;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toList;
import static li.strolch.model.StrolchModelConstants.ROLE_STROLCH_ADMIN;
import static li.strolch.rest.StrolchRestfulConstants.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/jobs")
@Tag(name = "Strolch Jobs", description = "Handles Strolch job management")
public class StrolchJobsResource {

	private static final Logger logger = LoggerFactory.getLogger(StrolchJobsResource.class);

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[1];
		return element.getClassName() + "." + element.getMethodName();
	}

	@Operation(summary = "Get all jobs", description = "Retrieves a list of all available jobs.", responses = {
			@ApiResponse(responseCode = "200", description = "Successfully retrieved jobs",
					content = @Content(mediaType = "application/json")),
			@ApiResponse(responseCode = "403", description = "Forbidden - insufficient privileges"),
			@ApiResponse(responseCode = "500", description = "Internal server error")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAll(@Context HttpServletRequest request, @Context HttpHeaders headers) {

		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		String source = (String) request.getAttribute(STROLCH_REQUEST_SOURCE);
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

						Privilege privilege = ctx.getPrivilege(StrolchJob.class.getName());
						return privilege.isAllAllowed() || privilege.getAllowList().contains(job.getClass().getName());
					}) //
					.sorted(comparing(StrolchJob::getName)) //
					.collect(toList());
			return ResponseUtil.listToResponse(DATA, jobs, StrolchJob::toJson);
		}
	}

	@Operation(summary = "Perform an action on a job",
			description = "Executes a specified action (run, schedule, cancel) on a job.", parameters = {
			@Parameter(name = "action", description = "Action to perform on the job", required = true,
					example = "runNow", schema = @Schema(allowableValues = {"runNow", "schedule", "cancel"}))},
			responses = {@ApiResponse(responseCode = "200", description = "Action executed successfully",
					content = @Content(mediaType = "application/json")),
					@ApiResponse(responseCode = "400", description = "Invalid action specified"),
					@ApiResponse(responseCode = "403", description = "Forbidden - insufficient privileges"),
					@ApiResponse(responseCode = "404", description = "Job not found"),
					@ApiResponse(responseCode = "500", description = "Internal server error")})
	@PUT
	@Produces(MediaType.APPLICATION_JSON)
	@Path("{name}/action")
	public Response doAction(@Context HttpServletRequest request, @Context HttpHeaders headers,
			@PathParam("name") String name, @QueryParam("action") String action) {

		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		String source = (String) request.getAttribute(STROLCH_REQUEST_SOURCE);

		ComponentContainer container = RestfulStrolchComponent.getInstance().getContainer();
		StrolchJobsHandler strolchJobsHandler = container.getComponent(StrolchJobsHandler.class);

		try {
			PrivilegeContext ctx = container.getPrivilegeHandler().validate(cert);

			if (name.equals(StrolchJobsHandler.class.getSimpleName())) {
				if (action.equals("reloadJobs")) {
					if (!ctx.hasRole(ROLE_STROLCH_ADMIN)) {
						SimpleRestrictable restrictable = new SimpleRestrictable(name, "reloadJobs");
						ctx.hasPrivilege(restrictable);
					}
					strolchJobsHandler.reloadJobs();
				} else {
					throw new IllegalArgumentException("Unhandled action " + action);
				}
			} else {

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
			}

			return ResponseUtil.toResponse();

		} catch (Exception e) {
			logger.error("Failed to perform job action {}", action, e);
			return ResponseUtil.toResponse(e);
		}
	}
}
