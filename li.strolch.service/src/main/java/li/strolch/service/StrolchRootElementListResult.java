package li.strolch.service;

import java.util.List;

import li.strolch.model.StrolchRootElement;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

public class StrolchRootElementListResult extends ServiceResult {

	private List<StrolchRootElement> rootElements;

	public StrolchRootElementListResult(ServiceResultState state) {
		super(state);
	}

	public StrolchRootElementListResult(ServiceResultState state, String msg) {
		super(state, msg);
	}

	public StrolchRootElementListResult(List<StrolchRootElement> rootElements) {
		super(ServiceResultState.SUCCESS);
		this.rootElements = rootElements;
	}

	public List<StrolchRootElement> getRootElements() {
		return this.rootElements;
	}
}
