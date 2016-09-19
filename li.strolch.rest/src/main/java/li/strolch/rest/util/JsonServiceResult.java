package li.strolch.rest.util;

import com.google.gson.JsonElement;

import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

/**
 * A {@link ServiceResult} which defines the result of an {@link AbstractService} to be a {@link JsonElement}. This is
 * often used in conjunction with REST APIs.
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class JsonServiceResult extends ServiceResult {
	private static final long serialVersionUID = 1L;
	private JsonElement result;

	public JsonServiceResult() {
		// do nothing
	}

	public JsonServiceResult(JsonElement result) {
		super(ServiceResultState.SUCCESS);
		this.result = result;
	}

	public JsonElement getResult() {
		return this.result;
	}
}
