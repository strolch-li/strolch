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
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogSeverity;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.rest.model.PagingResponse;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.search.ValueSearch;
import li.strolch.utils.collections.Paging;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Comparator.comparing;
import static li.strolch.search.SearchBuilder.buildSimpleValueSearch;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

@Path("strolch/operations-log")
@Tag(name = "Operations Log", description = "Endpoints for retrieving operations log messages.")
public class OperationsLogResource {

	@Operation(summary = "Get operation logs",
			description = "Retrieves operation logs for a given realm with optional filters for severity, date range, and search query.",
			responses = {@ApiResponse(responseCode = "200", description = "Operation logs retrieved successfully.",
					content = @Content(mediaType = "application/json",
							schema = @Schema(implementation = PagingResponse.class))),
					@ApiResponse(responseCode = "403", description = "Access denied."),
					@ApiResponse(responseCode = "500", description = "Internal server error.")})
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
			ValueSearch<LogMessage> valueSearch = buildSimpleValueSearch(new ValueSearch<>(), query,
					Arrays.asList(LogMessage::getMessage, m -> m.getLocator().toString()));
			messages = valueSearch.search(messages).asStream();
		}

		if (isNotEmpty(fromS) && isNotEmpty(toS)) {

			ZonedDateTime from = LocalDate.parse(fromS).atStartOfDay(ZoneId.systemDefault());
			ZonedDateTime to = LocalDate.parse(toS).plusDays(1).atStartOfDay(ZoneId.systemDefault());
			messages = messages.filter(logMessage -> from.isBefore(logMessage.getZonedDateTime()) && to.isAfter(
					logMessage.getZonedDateTime()));

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
