package li.strolch.rest.endpoint;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.StreamingOutput;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.SimpleRestrictable;
import li.strolch.report.Report;
import li.strolch.report.ReportElement;
import li.strolch.report.ReportSearch;
import li.strolch.rest.StrolchRestfulConstants;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.utils.ObjectHelper;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Comparator.comparing;
import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static li.strolch.report.ReportConstants.*;
import static li.strolch.rest.RestfulStrolchComponent.getInstance;
import static li.strolch.rest.StrolchRestfulConstants.PARAM_DATE_RANGE_SEL;
import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.utils.helper.StringHelper.*;
import static li.strolch.utils.iso8601.ISO8601.MAX_LOCAL_TIME;

@Path("strolch/reports")
public class ReportResource {

	private static final Logger logger = LoggerFactory.getLogger(ReportResource.class);

	private static String getContext() {
		StackTraceElement element = new Throwable().getStackTrace()[1];
		return element.getClassName() + "." + element.getMethodName();
	}

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

	@GET
	@Path("{id}/facets")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getReportFacets(@Context HttpServletRequest request, @QueryParam("realm") String realm,
			@PathParam("id") String id, @QueryParam(LIMIT) String limitS) throws IOException {

		Certificate cert = (Certificate) request.getAttribute(StrolchRestfulConstants.STROLCH_CERTIFICATE);
		if (isEmpty(realm))
			realm = getInstance().getContainer().getRealm(cert).getRealm();

		int limit = isNotEmpty(limitS) ? Integer.parseInt(limitS) : 10;

		File localesF = new File(request.getServletContext().getRealPath(LOCALES_JSON));
		JsonObject localeJ = null;
		if (localesF.exists()) {
			JsonObject localesJ = JsonParser
					.parseString(new String(Files.readAllBytes(localesF.toPath())))
					.getAsJsonObject();
			if (localesJ.has(cert.getLocale().toLanguageTag()))
				localeJ = localesJ.get(cert.getLocale().toLanguageTag()).getAsJsonObject();
		}

		long start = System.nanoTime();

		JsonObject result = new JsonObject();
		try (StrolchTransaction tx = getInstance().openTx(cert, realm, getContext());
			 Report report = new Report(tx, id)) {

			tx.getPrivilegeContext().validateAction(new SimpleRestrictable(ReportSearch.class.getName(), id));

			// set i18n data if possible
			if (localeJ != null)
				report.getReportPolicy().setI18nData(localeJ);

			JsonArray facetsJ = new JsonArray();
			JsonObject finalLocaleJ = localeJ;

			MapOfSets<String, JsonObject> criteria = report.generateFilterCriteria(limit);

			criteria.keySet().stream().sorted(comparing(type -> {
				JsonElement translatedJ = finalLocaleJ == null ? null : finalLocaleJ.get(type);
				return translatedJ == null ? type : translatedJ.getAsString();
			})).forEach(type -> {
				Set<JsonObject> elements = criteria.getSet(type);
				JsonObject filter = new JsonObject();
				filter.addProperty(Tags.Json.TYPE, type);
				filter.add(Tags.Json.VALUES, elements
						.stream()
						.sorted(comparing(e -> e.get(Tags.Json.NAME).getAsString()))
						.collect(JsonArray::new, JsonArray::add, JsonArray::addAll));
				facetsJ.add(filter);
			});

			String duration = formatNanoDuration(System.nanoTime() - start);

			result.add(PARAM_FACETS, facetsJ);
			result.addProperty(PARAM_DURATION, duration);
			result.addProperty(PARAM_PARALLEL, report.isParallel());

			logger.info("Facet Generation for {} took: {}", id, duration);
			return ResponseUtil.toResponse(DATA, result);
		}
	}

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

		File localesF = new File(request.getServletContext().getRealPath(LOCALES_JSON));
		JsonObject localeJ = null;
		if (localesF.exists()) {
			JsonObject localesJ = JsonParser
					.parseString(new String(Files.readAllBytes(localesF.toPath())))
					.getAsJsonObject();
			if (localesJ.has(cert.getLocale().toLanguageTag()))
				localeJ = localesJ.get(cert.getLocale().toLanguageTag()).getAsJsonObject();
		}

		long start = System.nanoTime();

		try (StrolchTransaction tx = getInstance().openTx(cert, realm, getContext());
			 Report report = new Report(tx, id)) {

			tx.getPrivilegeContext().validateAction(new SimpleRestrictable(ReportSearch.class.getName(), id));

			// set i18n data if possible
			if (localeJ != null)
				report.getReportPolicy().setI18nData(localeJ);

			// get filter criteria
			JsonArray array = report
					.generateFilterCriteria(type, limit, query)
					.sorted(comparing(e -> e.get(Tags.Json.NAME).getAsString()))
					.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);

			String duration = formatNanoDuration(System.nanoTime() - start);
			logger.info("Facet Generation for {}.{} took: {}", id, type, duration);
			return ResponseUtil.toResponse(DATA, array);
		}
	}

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

		// get information from body
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();

		int offset = jsonObject.get(OFFSET) != null ? jsonObject.get(OFFSET).getAsInt() : 50;
		int limit = jsonObject.get(LIMIT) != null ? jsonObject.get(LIMIT).getAsInt() : 50;

		MapOfSets<String, String> filters = jsonObject.get(PARAM_FILTER) != null ?
				getFiltersFromJson(jsonObject.get(PARAM_FILTER).getAsJsonArray()) : new MapOfSets<>();

		// get date range if defined
		JsonObject rangeJ = jsonObject.get(PARAM_DATE_RANGE) != null ? jsonObject.getAsJsonObject(PARAM_DATE_RANGE) :
				null;

		String fromS = rangeJ != null && rangeJ.get(PARAM_FROM) != null && !rangeJ.get(PARAM_FROM).isJsonNull() ?
				rangeJ.get(PARAM_FROM).getAsString() : null;
		ZonedDateTime from;
		try {
			from = fromS != null ? ISO8601.parseToZdt(fromS).with(LocalTime.MIN) : null;
		} catch (Exception e) {
			logger.error("Could not parse 'from' date, setting it to null.", e);
			from = null;
		}

		String toS = rangeJ != null && rangeJ.get(PARAM_TO) != null && !rangeJ.get(PARAM_TO).isJsonNull() ?
				rangeJ.get(PARAM_TO).getAsString() : null;
		ZonedDateTime to;
		try {
			to = (toS != null) ? ISO8601.parseToZdt(toS).with(MAX_LOCAL_TIME) : null;
		} catch (Exception e) {
			logger.error("Could not parse 'to' date, setting it to null.", e);
			to = null;
		}

		File localesF = new File(request.getServletContext().getRealPath(LOCALES_JSON));
		JsonObject localeJ = null;
		if (localesF.exists()) {
			JsonObject localesJ = JsonParser
					.parseString(new String(Files.readAllBytes(localesF.toPath())))
					.getAsJsonObject();
			if (localesJ.has(cert.getLocale().toLanguageTag()))
				localeJ = localesJ.get(cert.getLocale().toLanguageTag()).getAsJsonObject();
		}

		long start = System.nanoTime();

		try (StrolchTransaction tx = getInstance().openTx(cert, realm, getContext());
			 Report report = new Report(tx, id)) {

			tx.getPrivilegeContext().validateAction(new SimpleRestrictable(ReportSearch.class.getName(), id));

			// set i18n data if possible
			if (localeJ != null)
				report.getReportPolicy().setI18nData(localeJ);

			// add filters from request
			if (report.hasDateRangeSelector()) {
				DateRange dateRange = new DateRange();
				if (from != null)
					dateRange = dateRange.from(from, true);
				if (to != null)
					dateRange = dateRange.to(to, true);

				report.dateRange(dateRange);
			}

			if (!filters.isEmpty())
				filters.keySet().forEach(f -> report.filter(f, filters.getSet(f)));

			// get rows
			Stream<JsonObject> json = report.doReportWithPageAsJson(offset, limit);

			// add rows to response
			JsonObject finalResult = new JsonObject();
			JsonArray rows = new JsonArray();
			if (report.isParallel())
				json.forEachOrdered(rows::add);
			else
				json.forEach(rows::add);

			finalResult.add(PARAM_ROWS, rows);

			// add column information to JSON
			JsonArray col = new JsonArray();

			Resource reportR = tx.getResourceBy(TYPE_REPORT, id, true);
			reportR.getParameterBag(BAG_COLUMNS).getParameterKeySet().forEach(s -> {
				StringParameter param = reportR.getParameter(BAG_COLUMNS, s, true);

				JsonObject o = new JsonObject();
				o.addProperty(Tags.Json.ID, s);
				o.addProperty(Tags.Json.NAME, param.getName());
				o.addProperty(Tags.Json.INDEX, param.getIndex());
				col.add(o);
			});

			finalResult.add(PARAM_COLUMNS, col);

			long size = report.getCounter();
			long lastOffset = size % limit == 0 ? size - limit : (size / limit) * limit;
			long nextOffset = Math.min(lastOffset, limit + offset);
			long previousOffset = Math.max(0, offset - limit);

			finalResult.addProperty(LIMIT, limit);
			finalResult.addProperty(OFFSET, offset);
			finalResult.addProperty(SIZE, size);
			finalResult.addProperty(LAST_OFFSET, lastOffset);
			finalResult.addProperty(NEXT_OFFSET, nextOffset);
			finalResult.addProperty(PREVIOUS_OFFSET, previousOffset);

			String duration = formatNanoDuration(System.nanoTime() - start);
			finalResult.addProperty(PARAM_DURATION, duration);
			finalResult.addProperty(PARAM_PARALLEL, report.isParallel());

			logger.info("{} Report took: {}", id, duration);
			return ResponseUtil.toResponse(DATA, finalResult);
		}
	}

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
				getFiltersFromJson(jsonObject.get(PARAM_FILTER).getAsJsonArray()) : new MapOfSets<>();

		// get date range if defined
		JsonObject rangeJ = jsonObject != null && jsonObject.get(PARAM_DATE_RANGE) != null ?
				jsonObject.getAsJsonObject(PARAM_DATE_RANGE) : null;

		String fromS = rangeJ != null && rangeJ.get(PARAM_FROM) != null && !rangeJ.get(PARAM_FROM).isJsonNull() ?
				rangeJ.get(PARAM_FROM).getAsString() : null;
		ZonedDateTime from;
		try {
			from = fromS != null ? ISO8601.parseToZdt(fromS).with(LocalTime.MIN) : null;
		} catch (Exception e) {
			logger.error("Could not parse 'from' date, setting it to null.", e);
			from = null;
		}

		String toS = rangeJ != null && rangeJ.get(PARAM_TO) != null && !rangeJ.get(PARAM_TO).isJsonNull() ?
				rangeJ.get(PARAM_TO).getAsString() : null;
		ZonedDateTime to;
		try {
			to = (toS != null) ? ISO8601.parseToZdt(toS).with(MAX_LOCAL_TIME) : null;
		} catch (Exception e) {
			logger.error("Could not parse 'to' date, setting it to null.", e);
			to = null;
		}

		File localesF = new File(request.getServletContext().getRealPath(LOCALES_JSON));
		JsonObject localeJ = null;
		if (localesF.exists()) {
			JsonObject localesJ = JsonParser
					.parseString(new String(Files.readAllBytes(localesF.toPath())))
					.getAsJsonObject();
			if (localesJ.has(cert.getLocale().toLanguageTag()))
				localeJ = localesJ.get(cert.getLocale().toLanguageTag()).getAsJsonObject();
		}

		// create CSV printer with header
		StreamingOutput out = getOut(cert, realm, id, localeJ, filters, from, to);

		// send
		String fileName = id + "_" + System.currentTimeMillis() + ".csv";
		return Response
				.ok(out, TEXT_CSV_TYPE)
				.header("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
				.build();
	}

	private StreamingOutput getOut(Certificate cert, String realm, String reportId, JsonObject localeJ,
			MapOfSets<String, String> filters, ZonedDateTime from, ZonedDateTime to) {

		return out -> {

			try (StrolchTransaction tx = getInstance().openTx(cert, realm, getContext());
				 Report report = new Report(tx, reportId)) {

				tx.getPrivilegeContext().validateAction(new SimpleRestrictable(ReportSearch.class.getName(), reportId));

				// set i18n data if possible
				if (localeJ != null)
					report.getReportPolicy().setI18nData(localeJ);

				// add filters from request
				if (report.hasDateRangeSelector()) {
					DateRange dateRange = new DateRange();
					if (from != null)
						dateRange = dateRange.from(from, true);
					if (to != null)
						dateRange = dateRange.to(to, true);

					report.dateRange(dateRange);
				}

				// add filters from request
				filters.keySet().forEach(f -> report.filter(f, filters.getSet(f)));

				// get headers
				List<String> orderedColumnKeys = report.getColumnKeys();
				String[] headers = new String[orderedColumnKeys.size()];
				orderedColumnKeys.toArray(headers);

				if (localeJ != null) {
					for (int i = 0; i < headers.length; i++) {
						if (localeJ.has(headers[i]))
							headers[i] = localeJ.get(headers[i]).getAsString();
					}
				}

				// get report content and add to the buffer
				try (CSVPrinter csvP = new CSVPrinter(new OutputStreamWriter(out),
						CSVFormat.DEFAULT.builder().setHeader(headers).setDelimiter(';').build())) {

					if (report.isParallel())
						report.doReport().forEachOrdered(row -> writeCsv(csvP, row));
					else
						report.doReport().forEach(row -> writeCsv(csvP, row));
				}
			}
		};
	}

	private void writeCsv(CSVPrinter csvP, ReportElement row) {
		try {
			csvP.printRecord(row.valueStream().collect(Collectors.toList())); // add to CSV
		} catch (Exception e) {
			logger.error("Could not write CSV row", e);
		}
	}

	private MapOfSets<String, String> getFiltersFromJson(JsonArray filters) {
		MapOfSets<String, String> result = new MapOfSets<>();
		if (filters == null) {
			return result;
		}

		// go through all filters and add to the map of sets
		for (JsonElement elem : filters.getAsJsonArray()) {
			if (!elem.isJsonObject()) {
				logger.warn("There are wrong formatted filters:\n{}", elem);
				continue;
			}

			JsonObject filter = elem.getAsJsonObject();
			filter
					.get(PARAM_FACET_FILTERS)
					.getAsJsonArray()
					.forEach(f -> result.addElement(filter.get(PARAM_FACET_TYPE).getAsString(), f.getAsString()));
		}

		return result;
	}
}
