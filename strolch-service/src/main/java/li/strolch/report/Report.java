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

package li.strolch.report;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.privilege.model.SimpleRestrictable;
import li.strolch.report.policy.ReportPolicy;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.iso8601.ISO8601;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import static java.util.Comparator.comparing;
import static li.strolch.report.ReportConstants.*;
import static li.strolch.utils.helper.StringHelper.UTF8_BOM;
import static li.strolch.utils.helper.StringHelper.formatMillisecondsDuration;
import static li.strolch.utils.iso8601.ISO8601.MAX_LOCAL_TIME;

public class Report implements AutoCloseable {

	private static final Logger logger = LoggerFactory.getLogger(Report.class);

	private final ReportPolicy reportPolicy;

	public Report(StrolchTransaction tx, String reportId) {

		Resource reportRes = tx.getResourceBy(TYPE_REPORT, reportId, true);
		PolicyDef reportPolicyDef = reportRes.getPolicyDef(ReportPolicy.class.getSimpleName());

		long start = System.currentTimeMillis();
		PolicyHandler policyHandler = tx.getContainer().getComponent(PolicyHandler.class);
		this.reportPolicy = policyHandler.getPolicy(reportPolicyDef, tx);
		this.reportPolicy.initialize(reportId);
		long took = System.currentTimeMillis() - start;
		logger.info("Initializing report {} took {}", reportId, formatMillisecondsDuration(took));
	}

	public static void assertHasPrivilege(StrolchTransaction tx, String reportId) {
		tx.getPrivilegeContext().validateAction(new SimpleRestrictable(ReportSearch.class.getName(), reportId));
	}

	public Report assertHasPrivilege(StrolchTransaction tx) {
		assertHasPrivilege(tx, this.reportPolicy.getReportResource().getId());
		return this;
	}

	public ReportPolicy getReportPolicy() {
		return this.reportPolicy;
	}

	public Resource getReportResource() {
		return this.reportPolicy.getReportResource();
	}

	public boolean isParallel() {
		return this.reportPolicy.isParallel();
	}

	public Report i18nData(JsonObject i18nData) {
		this.reportPolicy.setI18nData(i18nData);
		return this;
	}

	public void setI18nData(JsonObject i18nData) {
		this.reportPolicy.setI18nData(i18nData);
	}

	public boolean hasDateRangeSelector() {
		return this.reportPolicy.hasDateRangeSelector();
	}

	public Report dateRange(DateRange dateRange) {
		this.reportPolicy.dateRange(dateRange);
		return this;
	}

	public Report dateRange(ZonedDateTime from, ZonedDateTime to) {
		if (!hasDateRangeSelector())
			return this;

		DateRange dateRange = new DateRange();
		if (from != null)
			dateRange = dateRange.from(from, true);
		if (to != null)
			dateRange = dateRange.to(to, true);

		return dateRange(dateRange);
	}

	public Report dateRange(JsonObject rangeJ) {
		if (rangeJ == null || !hasDateRangeSelector())
			return this;

		ZonedDateTime from = parseDateFrom(rangeJ);
		ZonedDateTime to = parseDateTo(rangeJ);
		return dateRange(from, to);
	}

	public static ZonedDateTime parseDateFrom(JsonObject rangeJ) {
		String fromS = rangeJ != null && rangeJ.get(PARAM_FROM) != null && !rangeJ.get(PARAM_FROM).isJsonNull() ?
				rangeJ.get(PARAM_FROM).getAsString() : null;
		ZonedDateTime from;
		try {
			from = fromS != null ? ISO8601.parseToZdt(fromS).with(LocalTime.MIN) : null;
		} catch (Exception e) {
			logger.error("Could not parse 'from' date, setting it to null.", e);
			from = null;
		}
		return from;
	}

	public static ZonedDateTime parseDateTo(JsonObject rangeJ) {
		String toS = rangeJ != null && rangeJ.get(PARAM_TO) != null && !rangeJ.get(PARAM_TO).isJsonNull() ?
				rangeJ.get(PARAM_TO).getAsString() : null;
		ZonedDateTime to;
		try {
			to = (toS != null) ? ISO8601.parseToZdt(toS).with(MAX_LOCAL_TIME) : null;
		} catch (Exception e) {
			logger.error("Could not parse 'to' date, setting it to null.", e);
			to = null;
		}
		return to;
	}

	public List<String> getColumnKeys() {
		return this.reportPolicy.getColumnKeys();
	}

	public Report filter(String type, String... ids) {
		this.reportPolicy.filter(type, ids);
		return this;
	}

	public Report filter(String type, List<String> ids) {
		this.reportPolicy.filter(type, ids);
		return this;
	}

	public Report filter(String type, Set<String> ids) {
		this.reportPolicy.filter(type, ids);
		return this;
	}

	public Report filter(MapOfSets<String, String> filters) {
		if (filters != null && !filters.isEmpty())
			filters.keySet().forEach(f -> filter(f, filters.getSet(f)));
		return this;
	}

	public Report filter(JsonArray filters) {
		if (filters == null || filters.isEmpty())
			return this;
		return filter(parseFiltersFromJson(filters));
	}

	public static MapOfSets<String, String> parseFiltersFromJson(JsonArray filters) {
		MapOfSets<String, String> result = new MapOfSets<>();
		if (filters == null)
			return result;

		for (JsonElement elem : filters) {
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

	public Stream<Map<String, StrolchRootElement>> buildStream() {
		return this.reportPolicy.buildStream();
	}

	public Stream<ReportElement> doReport() {
		return this.reportPolicy.doReport();
	}

	public Stream<ReportElement> doReportWithPage(int offset, int limit) {
		return this.reportPolicy.doReportWithPage(offset, limit);
	}

	public MapOfSets<String, JsonObject> generateFilterCriteria(int limit) {
		return this.reportPolicy.generateFilterCriteria(limit);
	}

	public Stream<JsonObject> generateFilterCriteria(String type, int limit, String query) {
		return this.reportPolicy.generateFilterCriteria(type, limit, query);
	}

	public JsonArray generateFacetsAsJson(int limit) {
		return generateFacetsAsJson(limit, null);
	}

	public JsonArray generateFacetsAsJson(int limit, JsonObject localeJ) {
		JsonArray facetsJ = new JsonArray();
		MapOfSets<String, JsonObject> criteria = generateFilterCriteria(limit);

		criteria.keySet().stream().sorted(comparing(type -> {
			JsonElement translatedJ = localeJ == null ? null : localeJ.get(type);
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

		return facetsJ;
	}

	public JsonArray generateFacetValuesAsJson(String type, int limit, String query) {
		return generateFilterCriteria(type, limit, query)
				.sorted(comparing(e -> e.get(Tags.Json.NAME).getAsString()))
				.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);
	}

	public long getCounter() {
		return this.reportPolicy.getCounter();
	}

	public Stream<JsonObject> doReportAsJson() {
		return doReport().map(e -> {
			JsonObject o = new JsonObject();
			e.keyValueStream().forEach(elem -> o.addProperty(elem.getKey(), elem.getValue()));
			return o;
		});
	}

	public Stream<JsonObject> doReportWithPageAsJson(int offset, int limit) {
		return doReportWithPage(offset, limit).map(e -> {
			JsonObject o = new JsonObject();
			e.keyValueStream().forEach(elem -> o.addProperty(elem.getKey(), elem.getValue()));
			return o;
		});
	}

	public JsonArray doReportAsJsonArray() {
		JsonArray rows = new JsonArray();
		Stream<JsonObject> json = doReportAsJson();
		if (isParallel())
			json.forEachOrdered(rows::add);
		else
			json.forEach(rows::add);
		return rows;
	}

	public JsonArray doReportWithPageAsJsonArray(int offset, int limit) {
		JsonArray rows = new JsonArray();
		Stream<JsonObject> json = doReportWithPageAsJson(offset, limit);
		if (isParallel())
			json.forEachOrdered(rows::add);
		else
			json.forEach(rows::add);
		return rows;
	}

	public JsonArray getColumnsAsJson() {
		return getColumnsAsJson(null);
	}

	public JsonArray getColumnsAsJson(JsonObject localeJ) {
		JsonArray col = new JsonArray();
		Resource reportR = getReportResource();
		if (reportR.hasParameterBag(BAG_COLUMNS)) {
			reportR.getParameterBag(BAG_COLUMNS).getParameterKeySet().forEach(s -> {
				StringParameter param = reportR.getParameter(BAG_COLUMNS, s, true);
				String name = param.getName();
				String columnName = localeJ != null && localeJ.has(name) ? localeJ.get(name).getAsString() : name;

				JsonObject o = new JsonObject();
				o.addProperty(Tags.Json.ID, s);
				o.addProperty(Tags.Json.NAME, columnName);
				o.addProperty(Tags.Json.INDEX, param.getIndex());
				col.add(o);
			});
		}
		return col;
	}

	public JsonObject generateReportWithPage(int offset, int limit) {
		return generateReportWithPage(offset, limit, null);
	}

	public JsonObject generateReportWithPage(int offset, int limit, JsonObject localeJ) {
		JsonArray rows = doReportWithPageAsJsonArray(offset, limit);
		JsonArray columns = getColumnsAsJson(localeJ);

		long size = getCounter();
		long lastOffset = size % limit == 0 ? size - limit : (size / limit) * limit;
		long nextOffset = Math.min(lastOffset, limit + offset);
		long previousOffset = Math.max(0, offset - limit);

		JsonObject result = new JsonObject();
		result.add(PARAM_ROWS, rows);
		result.add(PARAM_COLUMNS, columns);
		result.addProperty(PARAM_LIMIT, limit);
		result.addProperty(PARAM_OFFSET, offset);
		result.addProperty(PARAM_SIZE, size);
		result.addProperty(PARAM_LAST_OFFSET, lastOffset);
		result.addProperty(PARAM_NEXT_OFFSET, nextOffset);
		result.addProperty(PARAM_PREVIOUS_OFFSET, previousOffset);
		result.addProperty(PARAM_PARALLEL, isParallel());
		return result;
	}

	public String[] getHeaders() {
		return getHeaders(null);
	}

	public String[] getHeaders(JsonObject localeJ) {
		List<String> columnKeys = getColumnKeys();
		String[] headers = new String[columnKeys.size()];
		columnKeys.toArray(headers);

		if (localeJ != null) {
			for (int i = 0; i < headers.length; i++) {
				if (localeJ.has(headers[i]))
					headers[i] = localeJ.get(headers[i]).getAsString();
			}
		}

		return headers;
	}

	public void doReportAsCsv(OutputStream out) throws IOException {
		doReportAsCsv(out, null, ';', true);
	}

	public void doReportAsCsv(OutputStream out, JsonObject localeJ) throws IOException {
		doReportAsCsv(out, localeJ, ';', true);
	}

	public void doReportAsCsv(OutputStream out, JsonObject localeJ, char delimiter, boolean withBom)
			throws IOException {
		if (withBom)
			out.write(UTF8_BOM.getBytes(StandardCharsets.UTF_8));

		String[] headers = getHeaders(localeJ);
		try (CSVPrinter csvPrinter = new CSVPrinter(new OutputStreamWriter(out, StandardCharsets.UTF_8),
				CSVFormat.DEFAULT.builder().setHeader(headers).setDelimiter(delimiter).get())) {
			doReportAsCsv(csvPrinter);
		}
	}

	public void doReportAsCsv(Writer writer) throws IOException {
		doReportAsCsv(writer, null, ';');
	}

	public void doReportAsCsv(Writer writer, JsonObject localeJ) throws IOException {
		doReportAsCsv(writer, localeJ, ';');
	}

	public void doReportAsCsv(Writer writer, JsonObject localeJ, char delimiter) throws IOException {
		String[] headers = getHeaders(localeJ);
		try (CSVPrinter csvPrinter = new CSVPrinter(writer,
				CSVFormat.DEFAULT.builder().setHeader(headers).setDelimiter(delimiter).get())) {
			doReportAsCsv(csvPrinter);
		}
	}

	public void doReportAsCsv(File file) throws IOException {
		doReportAsCsv(file, null, ';', true);
	}

	public void doReportAsCsv(File file, JsonObject localeJ) throws IOException {
		doReportAsCsv(file, localeJ, ';', true);
	}

	public void doReportAsCsv(File file, JsonObject localeJ, char delimiter, boolean withBom) throws IOException {
		try (OutputStream out = Files.newOutputStream(file.toPath())) {
			doReportAsCsv(out, localeJ, delimiter, withBom);
		}
	}

	public void doReportAsCsv(CSVPrinter csvPrinter) {
		if (isParallel())
			doReport().forEachOrdered(row -> writeCsvRow(csvPrinter, row));
		else
			doReport().forEach(row -> writeCsvRow(csvPrinter, row));
	}

	public void writeCsvRow(CSVPrinter csvPrinter, ReportElement row) {
		try {
			csvPrinter.printRecord(row.valueStream().toList());
		} catch (Exception e) {
			logger.error("Could not write CSV row", e);
		}
	}

	@Override
	public void close() {
		try {
			this.reportPolicy.close();
		} catch (Exception e) {
			throw new IllegalStateException("Failed to close underlying policy " + this.reportPolicy.getClass(), e);
		}
	}
}
