package li.strolch.rest.endpoint;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.utils.collections.Paging;

@Path("strolch/operations-log")
public class OperationsLogResource {

	@GET
	@Path("{realm}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getOperationLog(@Context HttpServletRequest request, @QueryParam("offset") int offset,
			@QueryParam("limit") int limit, @PathParam("realm") String realm) {

		// TODO do privilege check

		OperationsLog operationsLog = RestfulStrolchComponent.getInstance().getComponent(OperationsLog.class);
		List<LogMessage> messages = operationsLog.getMessages(realm);

		Paging<LogMessage> paging = Paging.asPage(messages, offset, limit);
		return ResponseUtil.toResponse(paging, LogMessage::toJson);
	}
}
