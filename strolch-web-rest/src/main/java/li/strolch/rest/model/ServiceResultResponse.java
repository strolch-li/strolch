package li.strolch.rest.model;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.media.Schema;
import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

import static li.strolch.model.Tags.Json.*;
import static li.strolch.rest.StrolchRestfulConstants.I18N;
import static li.strolch.utils.helper.ExceptionHelper.formatException;
import static li.strolch.utils.helper.StringHelper.isEmpty;

@Schema(description = "Represents the result of a service execution.")
public class ServiceResultResponse {

	@Schema(description = "The state of the result.", example = "SUCCESS")
	private final ServiceResultState state;

	@Schema(description = "Message describing the result.", example = "Operation completed successfully.")
	private final String msg;

	@Schema(description = "Error message if an exception occurred.", nullable = true,
			example = "NullPointerException at line 42")
	private final String exceptionMsg;

	@Schema(description = "Stack trace of the exception if applicable.", nullable = true)
	private final String throwable;

	@Schema(description = "Internationalization message details if available.", nullable = true)
	private final I18nMessageResponse i18n;

	public ServiceResultResponse(ServiceResult serviceResult) {
		this.state = serviceResult.getState();
		this.msg = serviceResult.getMessage();

		Throwable throwable = serviceResult.getThrowable();
		if (throwable != null) {
			this.exceptionMsg = serviceResult.getRootMessage();
			this.throwable = formatException(throwable);

			this.i18n = throwable instanceof StrolchException ex && ex.hasI18n() ?
					new I18nMessageResponse(ex.getI18n()) : null;
		} else {
			this.exceptionMsg = null;
			this.throwable = null;
			this.i18n = serviceResult.getI18nMessage() == null ? null :
					new I18nMessageResponse(serviceResult.getI18nMessage());
		}
	}

	// Getters and Setters
	public ServiceResultState getState() {
		return state;
	}

	public String getMsg() {
		return msg;
	}

	public String getExceptionMsg() {
		return exceptionMsg;
	}

	public String getThrowable() {
		return throwable;
	}

	public Object getI18n() {
		return i18n;
	}

	public String toJson() {
		return new Gson().toJson(toJsonObject());
	}

	public JsonObject toJsonObject() {
		JsonObject json = new JsonObject();

		json.addProperty(Tags.Json.STATE, this.state.name());
		json.addProperty(MSG, isEmpty(this.msg) ? "-" : this.msg);

		if (this.throwable != null) {
			json.addProperty(EXCEPTION_MSG, this.exceptionMsg);
			json.addProperty(THROWABLE, this.throwable);
		}

		if (this.i18n != null)
			json.add(I18N, this.i18n.toJson());

		return json;
	}
}
