package li.strolch.service;

import li.strolch.model.StrolchRootElement;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class StrolchRootElementResult extends ServiceResult {

	private static final long serialVersionUID = 1L;

	private StrolchRootElement rootElement;

	public StrolchRootElementResult(StrolchRootElement rootElement) {
		super(ServiceResultState.SUCCESS);
		this.rootElement = rootElement;
	}

	public StrolchRootElement getRootElement() {
		return this.rootElement;
	}
}
