package li.strolch.report;

import static li.strolch.report.ReportConstants.TYPE_REPORT;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import com.google.gson.JsonObject;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.report.policy.ReportPolicy;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfSets;

public class Report implements AutoCloseable {

	private final ReportPolicy reportPolicy;

	public Report(StrolchTransaction tx, String reportId) {

		Resource reportRes = tx.getResourceBy(TYPE_REPORT, reportId, true);
		PolicyDef reportPolicyDef = reportRes.getPolicyDef(ReportPolicy.class.getSimpleName());

		PolicyHandler policyHandler = tx.getContainer().getComponent(PolicyHandler.class);
		this.reportPolicy = policyHandler.getPolicy(reportPolicyDef, tx);
		this.reportPolicy.initialize(reportId);
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

	public boolean hasDateRangeSelector() {
		return this.reportPolicy.hasDateRangeSelector();
	}

	public Report dateRange(DateRange dateRange) {
		this.reportPolicy.dateRange(dateRange);
		return this;
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

	public Stream<Map<String, StrolchRootElement>> buildStream() {
		return this.reportPolicy.buildStream();
	}

	public Stream<ReportElement> doReport() {
		return this.reportPolicy.doReport();
	}

	public Stream<ReportElement> doReportWithPage(int offset, int limit) {
		return this.reportPolicy.doReportWithPage(offset, limit);
	}

	public MapOfSets<String, StrolchRootElement> generateFilterCriteria(int limit) {
		return this.reportPolicy.generateFilterCriteria(limit);
	}

	public Stream<StrolchRootElement> generateFilterCriteria(String type) {
		return this.reportPolicy.generateFilterCriteria(type);
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

	@Override
	public void close() {
		try {
			this.reportPolicy.close();
		} catch (Exception e) {
			throw new IllegalStateException("Failed to close underlying policy " + this.reportPolicy.getClass(), e);
		}
	}
}
