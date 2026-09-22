/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.StreamingOutput;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.report.Report;
import li.strolch.report.ReportSearch;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.ZonedDateTime;

import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static li.strolch.report.Report.*;
import static li.strolch.report.ReportConstants.BAG_ADDITIONAL_TYPE;
import static li.strolch.report.ReportConstants.BAG_COLUMNS;
import static li.strolch.report.ReportConstants.BAG_JOINS;
import static li.strolch.report.ReportConstants.BAG_ORDERING;
import static li.strolch.report.ReportConstants.LOCALES_JSON;
import static li.strolch.report.ReportConstants.PARAM_DURATION;
import static li.strolch.report.ReportConstants.PARAM_PARALLEL;
import static li.strolch.report.ReportConstants.TYPE_FILTER;
import static li.strolch.rest.RestfulStrolchComponent.getInstance;
import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.utils.helper.ExceptionHelper.getCallerMethod;
import static li.strolch.utils.helper.StringHelper.*;

@Path("strolch/reports")
@Tag(name = "Report API", description = "API for managing reports")
public class ReportResource {

	private static final Logger logger = LoggerFactory.getLogger(ReportResource.class);

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[1];
		return element.getClassName() + "." + element.getMethodName();
	}

	@Operation(summary = "Get all report IDs", description = "Retrieves a list of all report IDs.", responses = {
			@ApiResponse(responseCode = "200", description = "Successful retrieval",
					content = @Content(mediaType = MediaType.APPLICATION_JSON)),
			@ApiResponse(responseCode = "500", description = "Internal server error")})
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAllReportIds(@Context HttpServletRequest request, @QueryParam("realm") String realm) {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		if (isEmpty(realm))
			realm = getInstance().getContainer().getRealm(cert).getRealm();

		try (StrolchTransaction tx = getInstance().openTx(cert, realm, getContext())) {

			StrolchRootElementToJsonVisitor visitor = new StrolchRootElementToJsonVisitor()
					.flat()
					.withoutVersion()
					.withoutObjectType()
					.withoutPolicies()
					.withoutStateVariables()
					.ignoreBags(BAG_JOINS, BAG_COLUMNS, BAG_ORDERING, BAG_ADDITIONAL_TYPE)
					.ignoreBagByType(TYPE_FILTER)
					.resourceHook((reportRes, reportJ) -> reportJ.addProperty(PARAM_DATE_RANGE,
							reportRes.hasParameter(BAG_PARAMETERS, PARAM_DATE_RANGE_SEL)));
			JsonArray result = new ReportSearch(tx)
					.search(tx)
					.orderByName(false)
					.map(resource -> resource.accept(visitor))
					.asStream()
					.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);

			return ResponseUtil.toResponse(DATA, result);
		}
	}

	@Operation(summary = "Get report facets", description = "Retrieves facets of a specific report.", responses = {
			@ApiResponse(responseCode = "200", description = "Successful retrieval",
					content = @Content(mediaType = MediaType.APPLICATION_JSON)),
			@ApiResponse(responseCode = "404", description = "Report not found"),
			@ApiResponse(responseCode = "500", description = "Internal server error")})
	@GET
	@Path("{id}/facets")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getReportFacets(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@PathParam("id") String id, @QueryParam(LIMIT) String limitS) throws IOException {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		if (isEmpty(realm))
			realm = getInstance().getContainer().getRealm(cert).getRealm();

		long start = System.nanoTime();
		int limit = isNotEmpty(limitS) ? Integer.parseInt(limitS) : 10;
		JsonObject localeJ = getI18nData(request, cert);
		JsonObject result = new JsonObject();

		try (StrolchTransaction tx = getInstance().openTx(cert, realm, getContext());
		     Report report = new Report(tx, id)) {
			report.assertHasPrivilege(tx);
			if (localeJ != null)
				report.i18nData(localeJ);

			JsonArray facetsJ = report.generateFacetsAsJson(limit, localeJ);

			String duration = formatNanoDuration(System.nanoTime() - start);

			result.add(PARAM_FACETS, facetsJ);
			result.addProperty(PARAM_DURATION, duration);
			result.addProperty(PARAM_PARALLEL, report.isParallel());

			logger.info("Facet Generation for {} took: {}", report.getReportResource().getId(), duration);
			return ResponseUtil.toResponse(DATA, result);
		}
	}

	@Operation(summary = "Get report facet values", description = "Retrieves specific facet values for a report.",
			responses = {@ApiResponse(responseCode = "200", description = "Successful retrieval",
					content = @Content(mediaType = MediaType.APPLICATION_JSON)),
					@ApiResponse(responseCode = "404", description = "Facet not found"),
					@ApiResponse(responseCode = "500", description = "Internal server error")})
	@GET
	@Path("{id}/facets/{type}/fields")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response getReportFacetValues(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@PathParam("id") String id, @PathParam("type") String type, @QueryParam(PARAM_QUERY) String queryS,
			@QueryParam(LIMIT) String limitS) throws IOException {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		if (isEmpty(realm))
			realm = getInstance().getContainer().getRealm(cert).getRealm();

		String query = isNotEmpty(queryS) ? queryS.toLowerCase() : queryS;
		int limit = isNotEmpty(limitS) ? Integer.parseInt(limitS) : 10;
		long start = System.nanoTime();
		JsonObject localeJ = getI18nData(request, cert);

		try (StrolchTransaction tx = getInstance().openTx(cert, realm, getContext());
		     Report report = new Report(tx, id)) {
			report.assertHasPrivilege(tx);
			if (localeJ != null)
				report.i18nData(localeJ);

			JsonArray array = report.generateFacetValuesAsJson(type, limit, query);

			String duration = formatNanoDuration(System.nanoTime() - start);
			logger.info("Facet Generation for {}.{} took: {}", report.getReportResource().getId(), type, duration);
			return ResponseUtil.toResponse(DATA, array);
		}
	}

	@Operation(summary = "Get report by ID", description = "Retrieves a report based on its ID.", responses = {
			@ApiResponse(responseCode = "200", description = "Successful retrieval",
					content = @Content(mediaType = MediaType.APPLICATION_JSON)),
			@ApiResponse(responseCode = "404", description = "Report not found"),
			@ApiResponse(responseCode = "500", description = "Internal server error")})
	@POST
	@Path("{id}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response getReportById(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@PathParam("id") String id, String data) throws IOException {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		if (isEmpty(realm))
			realm = getInstance().getContainer().getRealm(cert).getRealm();

		DBC.PRE.assertNotEmpty("report ID is required", id);
		long start = System.nanoTime();

		// get information from body
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();
		int offset = jsonObject.get(OFFSET) != null ? jsonObject.get(OFFSET).getAsInt() : 50;
		int limit = jsonObject.get(LIMIT) != null ? jsonObject.get(LIMIT).getAsInt() : 50;

		MapOfSets<String, String> filters = jsonObject.get(PARAM_FILTER) != null ?
				parseFiltersFromJson(jsonObject.get(PARAM_FILTER).getAsJsonArray()) : new MapOfSets<>();

		// get date range if defined
		JsonObject rangeJ = jsonObject.get(PARAM_DATE_RANGE) != null ? jsonObject.getAsJsonObject(PARAM_DATE_RANGE) :
				null;

		ZonedDateTime from = parseDateFrom(rangeJ);
		ZonedDateTime to = parseDateTo(rangeJ);
		JsonObject localeJ = getI18nData(request, cert);

		try (StrolchTransaction tx = getInstance().openTx(cert, realm, getContext());
		     Report report = new Report(tx, id)) {
			report.assertHasPrivilege(tx);
			if (localeJ != null)
				report.i18nData(localeJ);

			report.dateRange(from, to);
			report.filter(filters);

			JsonObject finalResult = report.generateReportWithPage(offset, limit, localeJ);

			String duration = formatNanoDuration(System.nanoTime() - start);
			finalResult.addProperty(PARAM_DURATION, duration);

			logger.info("{} Report took: {}", report.getReportResource().getId(), duration);
			return ResponseUtil.toResponse(DATA, finalResult);
		}
	}

	@Operation(summary = "Get report as CSV", description = "Retrieves a report as a CSV file.", responses = {
			@ApiResponse(responseCode = "200", description = "Successful retrieval",
					content = @Content(mediaType = TEXT_CSV)),
			@ApiResponse(responseCode = "404", description = "Report not found"),
			@ApiResponse(responseCode = "500", description = "Internal server error")})
	@POST
	@Path("{id}/csv")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(TEXT_CSV)
	public Response getReportByIdAsCsv(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@PathParam("id") String id, String data) throws IOException {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		if (isEmpty(realm))
			realm = getInstance().getContainer().getRealm(cert).getRealm();

		DBC.PRE.assertNotEmpty("report ID is required", id);

		// get information from body
		JsonObject jsonObject = StringHelper.isEmpty(data) ? null : JsonParser.parseString(data).getAsJsonObject();

		MapOfSets<String, String> filters = jsonObject != null && jsonObject.get(PARAM_FILTER) != null ?
				parseFiltersFromJson(jsonObject.get(PARAM_FILTER).getAsJsonArray()) : new MapOfSets<>();

		// get date range if defined
		JsonObject rangeJ = jsonObject != null && jsonObject.get(PARAM_DATE_RANGE) != null ?
				jsonObject.getAsJsonObject(PARAM_DATE_RANGE) : null;

		ZonedDateTime from = parseDateFrom(rangeJ);
		ZonedDateTime to = parseDateTo(rangeJ);
		JsonObject localeJ = getI18nData(request, cert);

		// create CSV printer with header
		StreamingOutput out = getOut(cert, realm, id, localeJ, filters, from, to, getCallerMethod(1));

		// send
		String fileName = id + "_" + System.currentTimeMillis() + ".csv";
		return Response
				.ok(out, TEXT_CSV_TYPE)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
				.build();
	}

	private StreamingOutput getOut(Certificate cert, String realm, String reportId, JsonObject localeJ,
			MapOfSets<String, String> filters, ZonedDateTime from, ZonedDateTime to, String action) {

		return out -> {
			try (StrolchTransaction tx = getInstance().openTx(cert, realm, action);
			     Report report = new Report(tx, reportId)) {

				report.assertHasPrivilege(tx);
				if (localeJ != null)
					report.i18nData(localeJ);

				report.dateRange(from, to);
				report.filter(filters);

				report.doReportAsCsv(out, localeJ);
			}
		};
	}

	private static JsonObject getI18nData(HttpServletRequest request, Certificate cert) throws IOException {
		File localesF = new File(request.getServletContext().getRealPath(LOCALES_JSON));
		JsonObject localeJ = null;
		if (localesF.exists()) {
			JsonObject localesJ = JsonParser
					.parseString(new String(Files.readAllBytes(localesF.toPath())))
					.getAsJsonObject();
			if (localesJ.has(cert.getLocale().toLanguageTag()))
				localeJ = localesJ.get(cert.getLocale().toLanguageTag()).getAsJsonObject();
		}
		return localeJ;
	}
}
