package li.strolch.service;

import li.strolch.model.ModelStatistics;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class XmlImportModelResult extends ServiceResult {
	private static final long serialVersionUID = 1L;

	private ModelStatistics statistics;

	public XmlImportModelResult(ModelStatistics statistics) {
		super(ServiceResultState.SUCCESS);
		this.statistics = statistics;
	}

	public XmlImportModelResult() {
		// no arg constructor
	}

	public XmlImportModelResult(ServiceResultState state, String message, Throwable throwable) {
		super(state, message, throwable);
	}

	public XmlImportModelResult(ServiceResultState state, String message) {
		super(state, message);
	}

	public XmlImportModelResult(ServiceResultState state) {
		super(state);
	}

	public ModelStatistics getStatistics() {
		return statistics;
	}
}
