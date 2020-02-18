package li.strolch.rest.endpoint;

import static li.strolch.execution.ExecutionHandler.PARAM_STATE;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Comparator;
import java.util.Objects;

import com.google.gson.JsonArray;
import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.ExecutionHandlerState;
import li.strolch.execution.service.*;
import li.strolch.model.Locator;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.model.json.StrolchElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.service.LocatorArgument;
import li.strolch.service.StringMapArgument;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;

@Path("strolch/control")
public class ControlResource {

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[2];
		return element.getClassName() + "." + element.getMethodName();
	}

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().openTx(certificate, realm, getContext());
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getActivities(@Context HttpServletRequest request, @QueryParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor().withVersion().withLocator();

		try (StrolchTransaction tx = openTx(cert, realm)) {
			ExecutionHandler executionHandler = tx.getContainer().getComponent(ExecutionHandler.class);
			JsonArray activitiesJ = executionHandler.getActiveActivitiesLocator(realm) //
					.stream() //
					.map(locator -> tx.getActivityBy(locator.get(1), locator.get(2))) //
					.filter(Objects::nonNull) //
					.sorted(Comparator.comparing(Activity::getId)) //
					.map(activity -> activity.accept(visitor)) //
					.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);

			ExecutionHandlerState state = executionHandler.getState(tx.getRealmName());
			return ResponseUtil.toResponse(PARAM_STATE, state.name(), activitiesJ);
		}
	}

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

	@GET
	@Path("executionHandler/state")
	public Response getExecutionHandlerState(@Context HttpServletRequest request, @QueryParam("realm") String realm) {

		ExecutionHandler executionHandler = RestfulStrolchComponent.getInstance().getComponent(ExecutionHandler.class);
		String state = executionHandler.getState(realm).name();

		return ResponseUtil.toResponse(PARAM_STATE, state);
	}

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

		switch (state) {
		case CREATED: {

			SetActionToCreatedService svc = new SetActionToCreatedService();
			svcResult = serviceHandler.doService(cert, svc, arg);

			break;
		}

		case PLANNED: {

			SetActionToPlannedService svc = new SetActionToPlannedService();
			svcResult = serviceHandler.doService(cert, svc, arg);

			break;
		}

		case EXECUTION: {

			ExecuteActionService svc = new ExecuteActionService();
			svcResult = serviceHandler.doService(cert, svc, arg);

			break;
		}

		case WARNING: {

			SetActionToWarningService svc = new SetActionToWarningService();
			svcResult = serviceHandler.doService(cert, svc, arg);

			break;
		}

		case ERROR: {

			SetActionToErrorService svc = new SetActionToErrorService();
			svcResult = serviceHandler.doService(cert, svc, arg);

			break;
		}

		case STOPPED: {

			SetActionToStoppedService svc = new SetActionToStoppedService();
			svcResult = serviceHandler.doService(cert, svc, arg);

			break;
		}

		case EXECUTED: {

			SetActionToExecutedService svc = new SetActionToExecutedService();
			svcResult = serviceHandler.doService(cert, svc, arg);

			break;
		}

		case CLOSED: {

			SetActionToClosedService svc = new SetActionToClosedService();
			svcResult = serviceHandler.doService(cert, svc, arg);

			break;
		}

		default:
			throw new UnsupportedOperationException("Unhandled state " + state);
		}

		return ResponseUtil.toResponse(svcResult);
	}

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
