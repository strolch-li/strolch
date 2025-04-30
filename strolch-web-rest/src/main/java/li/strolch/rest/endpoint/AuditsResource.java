/*
 * Copyright (c) 2015-2024 Robert von Burg <eitch@eitchnet.ch>
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
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.json.AuditToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.Paging;
import li.strolch.utils.time.Interval;
import li.strolch.utils.time.PeriodDuration;

import java.time.Duration;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.stream.Stream;

import static java.util.Comparator.comparing;
import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;
import static li.strolch.utils.helper.ExceptionHelper.getCallerMethod;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;
import static li.strolch.utils.iso8601.ISO8601.parseToZdt;

@Path("strolch/audits")
@Tag(name = "Audits Resource", description = "Manage and query audit logs.")
public class AuditsResource {

	private StrolchTransaction openTx(Certificate certificate, String realm) {
		return RestfulStrolchComponent.getInstance().openTx(certificate, realm, getCallerMethod());
	}

	@Operation(summary = "Get audits",
			description = "Retrieves a paginated list of audits filtered by optional criteria.", responses = {
			@ApiResponse(responseCode = "200", description = "List of audits.",
					content = @Content(mediaType = "application/json", examples = @ExampleObject(
							"{\"paging\": {\"offset\": 0, \"limit\": 50, \"total\": 100}, \"data\": [...] }"))),
			@ApiResponse(responseCode = "400", description = "Invalid input parameters."),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Path("{realm}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAudits(@Context HttpServletRequest request, @PathParam("realm") String realm,
			@QueryParam("offset") @DefaultValue("0") int offset, @QueryParam("limit") @DefaultValue("50") int limit,
			@QueryParam("from") String fromS, @QueryParam("to") String toS, @QueryParam("username") String username,
			@QueryParam("elementType") String elementType, @QueryParam("elementSubType") String elementSubType,
			@QueryParam("elementAccessed") String elementAccessed, @QueryParam("action") String action,
			@QueryParam("accessType") String accessType) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		PrivilegeHandler privilegeHandler = RestfulStrolchComponent.getInstance().getContainer().getPrivilegeHandler();
		PrivilegeContext ctx = privilegeHandler.getPrivilegeHandler().validate(cert);
		ctx.assertHasPrivilege(AuditTrail.class.getSimpleName());

		DateRange dateRange = new DateRange();
		if (isEmpty(fromS) && isEmpty(toS)) {
			ZonedDateTime now = ZonedDateTime.now().truncatedTo(ChronoUnit.DAYS);
			dateRange.from(now);
			dateRange.to(now.plusDays(1));
		} else {
			if (isNotEmpty(fromS))
				dateRange.from(parseToZdt(fromS), true);
			if (isNotEmpty(toS))
				dateRange.to(parseToZdt(toS), false);
		}

		if (dateRange.isUnbounded())
			throw new IllegalArgumentException(
					"The date range must be bounded and the date range must be at least 1 day long and at most 30 days long.");
		Interval interval = dateRange.toInterval();
		if (PeriodDuration.of(interval.toDuration()).toMillis() > Duration.ofDays(35).toMillis())
			throw new IllegalArgumentException(
					"The duration of the date range is too long. Please use a date range of at most 30 days.");

		Paging<Audit> paging;
		try (StrolchTransaction tx = openTx(cert, realm)) {
			if (!tx.isAuditTrailEnabled())
				return ResponseUtil.toResponse(DATA, new JsonArray());

			AuditTrail auditTrail = tx.getAuditTrail();

			long totalAudits = auditTrail.querySize(tx);
			List<Audit> auditsUnfiltered;

			if (isEmpty(elementType))
				auditsUnfiltered = auditTrail.getAllElements(tx, dateRange);
			else
				auditsUnfiltered = auditTrail.getAllElements(tx, elementType, dateRange);
			Stream<Audit> audits = auditsUnfiltered.stream().sorted(comparing(Audit::getDate).reversed());

			if (isNotEmpty(username))
				audits = audits.filter(audit -> audit.getUsername().equals(username));
			if (isNotEmpty(elementSubType))
				audits = audits.filter(audit -> audit.getElementSubType().equals(elementSubType));
			if (isNotEmpty(elementAccessed))
				audits = audits.filter(audit -> audit.getElementAccessed().equals(elementAccessed));
			if (isNotEmpty(action))
				audits = audits.filter(audit -> audit.getAction().equals(action));
			if (isNotEmpty(accessType)) {
				AccessType accessType1 = AccessType.valueOf(accessType);
				audits = audits.filter(audit -> audit.getAccessType().equals(accessType1));
			}

			if (offset > auditsUnfiltered.size())
				offset = Math.max(0, auditsUnfiltered.size() - limit);

			paging = Paging.asPage(audits.toList(), offset, limit);
			paging.setDataSetSize(totalAudits);
		}

		AuditToJsonVisitor toJsonVisitor = new AuditToJsonVisitor().withAdditionalData();
		return ResponseUtil.toResponse(paging, toJsonVisitor::visitAudit);
	}

	@Operation(summary = "Get audit types", description = "Retrieves all available audit types.", responses = {
			@ApiResponse(responseCode = "200", description = "List of audit types.",
					content = @Content(mediaType = "application/json",
							examples = @ExampleObject("[\"Type1\",\"Type2\"]"))),
			@ApiResponse(responseCode = "500", description = "Internal server error.")})
	@GET
	@Path("{realm}/types")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response queryTypes(@Context HttpServletRequest request, @PathParam("realm") String realm) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		try (StrolchTransaction tx = openTx(cert, realm)) {
			JsonArray dataJ = new JsonArray();
			if (tx.isAuditTrailEnabled())
				tx.getAuditTrail().getTypes(tx).forEach(dataJ::add);
			return Response.ok(dataJ.toString(), MediaType.APPLICATION_JSON).build();
		}
	}
}
