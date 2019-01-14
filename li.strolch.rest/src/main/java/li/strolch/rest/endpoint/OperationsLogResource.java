package li.strolch.rest.endpoint;

import static java.util.Comparator.comparing;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.collections.Paging;

@Path("strolch/operations-log")
public class OperationsLogResource {

	@GET
	@Path("{realm}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getOperationLog(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@QueryParam("offset") int offset, @QueryParam("limit") int limit, @QueryParam("severity") String severityS,
			@QueryParam("exactSeverity") Boolean exactSeverity, @QueryParam("from") String fromS,
			@QueryParam("to") String toS, @QueryParam("query") String query) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = RestfulStrolchComponent.getInstance().getContainer().getPrivilegeHandler();
		PrivilegeContext ctx = privilegeHandler.getPrivilegeHandler().validate(cert);
		ctx.assertHasPrivilege(OperationsLog.class.getName());

		OperationsLog operationsLog = RestfulStrolchComponent.getInstance().getComponent(OperationsLog.class);
		List<LogMessage> allMessages = operationsLog.getMessages(realm);
		Stream<LogMessage> messages = allMessages.stream();

		if (isNotEmpty(severityS)) {
			LogSeverity severity = LogSeverity.valueOf(severityS);
			messages = messages.filter(logMessage -> {
				if (exactSeverity != null && exactSeverity)
					return logMessage.getSeverity().equals(severity);
				else
					return logMessage.getSeverity().compareTo(severity) >= 0;
			});
		}

		if (isNotEmpty(query)) {
			messages = messages.filter(logMessage -> logMessage.getMessage().toLowerCase().contains(query) //
					|| logMessage.getLocator().getPathElements().contains(query));
		}

		if (isNotEmpty(fromS) && isNotEmpty(toS)) {

			ZonedDateTime from = LocalDate.parse(fromS).atStartOfDay(ZoneId.systemDefault());
			ZonedDateTime to = LocalDate.parse(toS).plusDays(1).atStartOfDay(ZoneId.systemDefault());
			messages = messages.filter(logMessage -> from.isBefore(logMessage.getZonedDateTime()) && to
					.isAfter(logMessage.getZonedDateTime()));

		} else if (isNotEmpty(fromS)) {

			ZonedDateTime from = LocalDate.parse(fromS).atStartOfDay(ZoneId.systemDefault());
			messages = messages.filter(logMessage -> from.isBefore(logMessage.getZonedDateTime()));

		} else if (isNotEmpty(toS)) {

			ZonedDateTime to = LocalDate.parse(toS).plusDays(1).atStartOfDay(ZoneId.systemDefault());
			messages = messages.filter(logMessage -> to.isAfter(logMessage.getZonedDateTime()));

		}

		messages = messages.sorted(comparing(LogMessage::getId).reversed());

		Paging<LogMessage> paging = Paging.asPage(messages.collect(Collectors.toList()), offset, limit);
		paging.setDataSetSize(allMessages.size());
		return ResponseUtil.toResponse(paging, LogMessage::toJson);
	}
}
