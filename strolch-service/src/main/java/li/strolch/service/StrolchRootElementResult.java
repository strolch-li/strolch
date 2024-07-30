package li.strolch.service;

import li.strolch.model.StrolchRootElement;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class StrolchRootElementResult extends ServiceResult {

	private StrolchRootElement rootElement;

	public StrolchRootElementResult(ServiceResultState state) {
		super(state);
	}

	public StrolchRootElementResult(ServiceResultState state, String msg) {
		super(state, msg);
	}

	public StrolchRootElementResult(StrolchRootElement rootElement) {
		super(ServiceResultState.SUCCESS);
		this.rootElement = rootElement;
	}

	public StrolchRootElement getRootElement() {
		return this.rootElement;
	}
}
