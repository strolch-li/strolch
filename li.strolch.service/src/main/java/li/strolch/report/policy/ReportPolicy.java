package li.strolch.report.policy;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import com.google.gson.JsonObject;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;
import li.strolch.report.ReportElement;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfSets;

public abstract class ReportPolicy extends StrolchPolicy {

	public ReportPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public abstract boolean isParallel();

	public abstract void setI18nData(JsonObject i18nData);

	public abstract void initialize(String reportId);

	public abstract boolean hasDateRangeSelector();

	public abstract Resource getReportResource();

	public abstract ReportPolicy dateRange(DateRange dateRange);

	public abstract List<String> getColumnKeys();

	public abstract ReportPolicy filter(String type, String... ids);

	public abstract ReportPolicy filter(String type, List<String> ids);

	public abstract ReportPolicy filter(String type, Set<String> ids);

	public abstract Stream<Map<String, StrolchRootElement>> buildStream();

	public abstract Stream<Map<String, StrolchRootElement>> buildStream(boolean withOrdering);

	public abstract Stream<ReportElement> doReport();

	public abstract Stream<ReportElement> doReportWithPage(int offset, int limit);

	public abstract MapOfSets<String, StrolchRootElement> generateFilterCriteria(int limit);

	public abstract Stream<StrolchRootElement> generateFilterCriteria(String type);

	public abstract long getCounter();

	public abstract boolean withPage();

	public abstract int getOffset();

	public abstract int getLimit();

	public void close() throws Exception {
		// do nothing
	}
}
