/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.ExecutionHandlerState;
import li.strolch.execution.service.*;
import li.strolch.model.Locator;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.model.ServiceResultResponse;
import li.strolch.rest.model.StrolchResponse;
import li.strolch.service.LocatorArgument;
import li.strolch.service.StringMapArgument;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;

import java.util.Comparator;
import java.util.Objects;

import static li.strolch.execution.ExecutionHandler.PARAM_STATE;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;
import static li.strolch.rest.model.ToJsonHelper.inExecutionActivityToJson;

@Path("strolch/control")
@Tag(name = "Control Resource", description = "Manage execution activities and states.")
public class ControlResource {

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[2];
		return element.getClassName() + "." + element.getMethodName();
	}

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().openTx(certificate, realm, getContext());
	}

	@Operation(summary = "Get active execution activities",
			description = "Retrieves all currently active execution activities in the given realm.")
	@ApiResponse(responseCode = "200", description = "List of active execution activities.",
			content = @Content(mediaType = "application/json",
					schema = @Schema(implementation = StrolchResponse.class)))
	@ApiResponse(responseCode = "500", description = "Internal server error.")
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getActivities(@Context HttpServletRequest request, @QueryParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		try (StrolchTransaction tx = openTx(cert, realm)) {
			ExecutionHandler executionHandler = tx.getContainer().getComponent(ExecutionHandler.class);
			JsonArray activitiesJ = executionHandler
					.getActiveActivitiesLocator(realm)
					.stream()
					.map(locator -> tx.getActivityBy(locator.get(1), locator.get(2)))
					.filter(Objects::nonNull)
					.sorted(Comparator.comparing(Activity::getId))
					.map(activity -> activity.accept(inExecutionActivityToJson(tx.getRealmName(), executionHandler)))
					.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);

			ExecutionHandlerState state = executionHandler.getExecutionState(tx.getRealmName());
			return ResponseUtil.toResponse(PARAM_STATE, state.name(), activitiesJ);
		}
	}

	@Operation(summary = "Clear all execution activities",
			description = "Removes all currently running execution activities.")
	@ApiResponse(responseCode = "200", description = "All execution activities cleared.",
			content = @Content(mediaType = "application/json",
					schema = @Schema(implementation = ServiceResultResponse.class)))
	@ApiResponse(responseCode = "500", description = "Internal server error.")
	@DELETE
	@Path("all")
	public Response clearAllActivities(@Context HttpServletRequest request, @QueryParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		RestfulStrolchComponent instance = RestfulStrolchComponent.getInstance();

		ClearAllCurrentExecutionsService svc = new ClearAllCurrentExecutionsService();
		ServiceArgument arg = svc.getArgumentInstance();
		arg.realm = realm;

		ServiceResult svcResult = instance.getServiceHandler().doService(cert, svc, arg);
		return ResponseUtil.toResponse(svcResult);
	}

	@Operation(summary = "Get execution handler state",
			description = "Retrieves the current state of the execution handler.")
	@ApiResponse(responseCode = "200", description = "Execution handler state returned.",
			content = @Content(mediaType = "application/json",
					schema = @Schema(implementation = StrolchResponse.class)))
	@ApiResponse(responseCode = "500", description = "Internal server error.")
	@GET
	@Path("executionHandler/state")
	public Response getExecutionHandlerState(@QueryParam("realm") String realm) {
		ExecutionHandler executionHandler = RestfulStrolchComponent.getInstance().getComponent(ExecutionHandler.class);
		String state = executionHandler.getExecutionState(realm).name();

		return ResponseUtil.toResponse(PARAM_STATE, state);
	}

	@Operation(summary = "Set execution handler state", description = "Sets the state of the execution handler.")
	@ApiResponse(responseCode = "200", description = "Execution handler state updated.",
			content = @Content(mediaType = "application/json",
					schema = @Schema(implementation = ServiceResultResponse.class)))
	@ApiResponse(responseCode = "400", description = "Invalid state value.")
	@ApiResponse(responseCode = "500", description = "Internal server error.")
	@PUT
	@Path("executionHandler/state")
	public Response setExecutionHandlerState(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@QueryParam("state") String stateS) {

		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		SetExecutionHandlerStateService svc = new SetExecutionHandlerStateService();
		StringMapArgument arg = svc.getArgumentInstance();
		arg.realm = realm;
		arg.map.put("state", stateS);

		ServiceHandler serviceHandler = RestfulStrolchComponent.getInstance().getServiceHandler();
		ServiceResult svcResult = serviceHandler.doService(cert, svc, arg);
		return ResponseUtil.toResponse(svcResult);
	}

	@Operation(summary = "Execute an activity", description = "Starts execution of the specified activity.")
	@ApiResponse(responseCode = "200", description = "Activity execution started successfully.",
			content = @Content(mediaType = "application/json",
					schema = @Schema(implementation = ServiceResultResponse.class)))
	@ApiResponse(responseCode = "400", description = "Invalid parameters provided.")
	@ApiResponse(responseCode = "403", description = "Access denied.")
	@ApiResponse(responseCode = "500", description = "Internal server error.")
	@POST
	@Path("activity/state")
	public Response executeActivity(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@QueryParam("type") String type, @QueryParam("id") String id, @QueryParam("state") String stateS) {

		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		Locator locator = Activity.locatorFor(type, id);
		RestfulStrolchComponent instance = RestfulStrolchComponent.getInstance();

		StartActivityExecutionService svc = new StartActivityExecutionService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.locator = locator;

		ServiceResult svcResult = instance.getServiceHandler().doService(cert, svc, arg);
		return ResponseUtil.toResponse(svcResult);
	}

	@Operation(summary = "Set activity state", description = "Updates the state of an activity element.")
	@ApiResponse(responseCode = "200", description = "Activity state updated successfully.",
			content = @Content(mediaType = "application/json",
					schema = @Schema(implementation = ServiceResultResponse.class)))
	@ApiResponse(responseCode = "400", description = "Invalid state or locator provided.")
	@ApiResponse(responseCode = "403", description = "Access denied.")
	@ApiResponse(responseCode = "500", description = "Internal server error.")
	@PUT
	@Path("activity/state")
	public Response setElementState(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@QueryParam("locator") String locatorS, @QueryParam("state") String stateS) {

		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		State state = State.parse(stateS);
		Locator locator = Locator.valueOf(locatorS);

		LocatorArgument arg = new LocatorArgument();
		arg.locator = locator;

		ServiceHandler serviceHandler = RestfulStrolchComponent.getInstance().getServiceHandler();
		ServiceResult svcResult;
		Service<LocatorArgument, ServiceResult> svc;

		switch (state) {
			case CREATED -> svc = new SetActionToCreatedService();
			case PLANNED -> svc = new SetActionToPlannedService();
			case EXECUTION -> svc = new ExecuteActionService();
			case WARNING -> svc = new SetActionToWarningService();
			case ERROR -> svc = new SetActionToErrorService();
			case STOPPED -> svc = new SetActionToStoppedService();
			case EXECUTED -> svc = new SetActionToExecutedService();
			case CLOSED -> svc = new SetActionToClosedService();
			default -> throw new UnsupportedOperationException("Unhandled state " + state);
		}

		svcResult = serviceHandler.doService(cert, svc, arg);
		return ResponseUtil.toResponse(svcResult);
	}

	@Operation(summary = "Remove activity from execution",
			description = "Stops execution of an activity and removes it from tracking.")
	@ApiResponse(responseCode = "200", description = "Activity successfully removed from execution.",
			content = @Content(mediaType = "application/json",
					schema = @Schema(implementation = ServiceResultResponse.class)))
	@ApiResponse(responseCode = "400", description = "Invalid activity parameters provided.")
	@ApiResponse(responseCode = "403", description = "Access denied.")
	@ApiResponse(responseCode = "500", description = "Internal server error.")
	@DELETE
	@Path("activity/state")
	public Response removeActivityFromExecution(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@QueryParam("type") String type, @QueryParam("id") String id) {

		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		RestfulStrolchComponent instance = RestfulStrolchComponent.getInstance();
		Locator locator = Activity.locatorFor(type, id);

		RemoveActivityFromExecutionService svc = new RemoveActivityFromExecutionService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.realm = realm;
		arg.locator = locator;

		ServiceResult svcResult = instance.getServiceHandler().doService(cert, svc, arg);
		return ResponseUtil.toResponse(svcResult);
	}
}
