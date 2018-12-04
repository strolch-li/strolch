package li.strolch.rest.endpoint;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.google.gson.JsonElement;
import li.strolch.execution.ExecutionHandler;
import li.strolch.execution.service.*;
import li.strolch.model.Locator;
import li.strolch.model.State;
import li.strolch.model.activity.Activity;
import li.strolch.model.json.StrolchElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.ServiceArgument;
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

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor().withVersion().withLocator();

		List<JsonElement> activities;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			activities = tx.getContainer().getComponent(ExecutionHandler.class).getActiveActivitiesLocator(realm)
					.stream().map(locator -> tx.getActivityBy(locator.get(1), locator.get(2))).filter(Objects::nonNull)
					.sorted(Comparator.comparing(Activity::getId)).map(activity -> activity.accept(visitor))
					.collect(Collectors.toList());
		}

		return ResponseUtil.toResponse(activities);
	}

	@DELETE
	@Path("all")
	public Response clearAllActivities(@Context HttpServletRequest request, @QueryParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		RestfulStrolchComponent instance = RestfulStrolchComponent.getInstance();

		ClearAllCurrentExecutionsService svc = new ClearAllCurrentExecutionsService();
		ServiceArgument arg = svc.getArgumentInstance();
		arg.realm = realm;

		ServiceResult svcResult = instance.getServiceHandler().doService(cert, svc, arg);

		return ResponseUtil.toResponse(svcResult);
	}

	@POST
	@Path("state")
	public Response executeActivity(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@QueryParam("locator") String locatorS, @QueryParam("state") String stateS) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		Locator locator = Locator.valueOf(locatorS);

		RestfulStrolchComponent instance = RestfulStrolchComponent.getInstance();

		StartActivityExecutionService svc = new StartActivityExecutionService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.locator = locator;

		ServiceResult svcResult = instance.getServiceHandler().doService(cert, svc, arg);

		return ResponseUtil.toResponse(svcResult);
	}

	@DELETE
	@Path("state")
	public Response removeActivityFromExecution(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@QueryParam("locator") String locatorS) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		RestfulStrolchComponent instance = RestfulStrolchComponent.getInstance();

		Locator locator = Locator.valueOf(locatorS);

		RemoveActivityFromExecutionService svc = new RemoveActivityFromExecutionService();
		LocatorArgument arg = svc.getArgumentInstance();
		arg.realm = realm;
		arg.locator = locator;

		ServiceResult svcResult = instance.getServiceHandler().doService(cert, svc, arg);

		return ResponseUtil.toResponse(svcResult);
	}

	@PUT
	@Path("state")
	public Response setElementState(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@QueryParam("locator") String locatorS, @QueryParam("state") String stateS) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);

		RestfulStrolchComponent instance = RestfulStrolchComponent.getInstance();

		if (stateS.equals("Trigger")) {

			TriggerExecutionForRealmService svc = new TriggerExecutionForRealmService();
			ServiceArgument arg = svc.getArgumentInstance();
			arg.realm = realm;

			ServiceResult svcResult = instance.getServiceHandler().doService(cert, svc, arg);
			return ResponseUtil.toResponse(svcResult);

		} else if (stateS.equals("ReloadActivities")) {

			ReloadActivitiesInExecutionService svc = new ReloadActivitiesInExecutionService();
			ServiceArgument arg = svc.getArgumentInstance();
			arg.realm = realm;

			ServiceResult svcResult = instance.getServiceHandler().doService(cert, svc, arg);
			return ResponseUtil.toResponse(svcResult);
		}

		State state = State.parse(stateS);
		Locator locator = Locator.valueOf(locatorS);

		ServiceResult svcResult;
		switch (state) {
		case CREATED: {

			SetActionToCreatedService svc = new SetActionToCreatedService();
			LocatorArgument arg = svc.getArgumentInstance();
			arg.realm = realm;
			arg.locator = locator;

			svcResult = instance.getServiceHandler().doService(cert, svc, arg);

			break;
		}

		case PLANNING: {

			SetActionToPlanningService svc = new SetActionToPlanningService();
			LocatorArgument arg = svc.getArgumentInstance();
			arg.realm = realm;
			arg.locator = locator;

			svcResult = instance.getServiceHandler().doService(cert, svc, arg);

			break;
		}

		case PLANNED: {

			SetActionToPlannedService svc = new SetActionToPlannedService();
			LocatorArgument arg = svc.getArgumentInstance();
			arg.realm = realm;
			arg.locator = locator;

			svcResult = instance.getServiceHandler().doService(cert, svc, arg);

			break;
		}

		case EXECUTION: {

			SetToExecutionService svc = new SetToExecutionService();
			LocatorArgument arg = svc.getArgumentInstance();
			arg.realm = realm;
			arg.locator = locator;

			svcResult = instance.getServiceHandler().doService(cert, svc, arg);

			break;
		}

		case WARNING: {

			SetActionToWarningService svc = new SetActionToWarningService();
			LocatorArgument arg = svc.getArgumentInstance();
			arg.realm = realm;
			arg.locator = locator;

			svcResult = instance.getServiceHandler().doService(cert, svc, arg);

			break;
		}

		case ERROR: {

			SetActionToErrorService svc = new SetActionToErrorService();
			LocatorArgument arg = svc.getArgumentInstance();
			arg.realm = realm;
			arg.locator = locator;

			svcResult = instance.getServiceHandler().doService(cert, svc, arg);

			break;
		}

		case STOPPED: {

			SetActionToStoppedService svc = new SetActionToStoppedService();
			LocatorArgument arg = svc.getArgumentInstance();
			arg.realm = realm;
			arg.locator = locator;

			svcResult = instance.getServiceHandler().doService(cert, svc, arg);

			break;
		}

		case EXECUTED: {

			SetActionToExecutedService svc = new SetActionToExecutedService();
			LocatorArgument arg = svc.getArgumentInstance();
			arg.realm = realm;
			arg.locator = locator;

			svcResult = instance.getServiceHandler().doService(cert, svc, arg);

			break;
		}

		case CLOSED: {

			SetActionToClosedService svc = new SetActionToClosedService();
			LocatorArgument arg = svc.getArgumentInstance();
			arg.realm = realm;
			arg.locator = locator;

			svcResult = instance.getServiceHandler().doService(cert, svc, arg);

			break;
		}

		default:
			throw new UnsupportedOperationException("Unhandled state " + state);
		}

		return ResponseUtil.toResponse(svcResult);
	}
}
