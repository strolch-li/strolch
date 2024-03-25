/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.service.api;

import static li.strolch.model.Tags.Json.*;
import static li.strolch.utils.helper.ExceptionHelper.formatException;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessageWithCauses;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.util.Locale;
import java.util.ResourceBundle;

import com.google.gson.JsonObject;
import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchUserMessageException;
import li.strolch.model.Tags;
import li.strolch.model.i18n.I18nMessageToJsonVisitor;
import li.strolch.utils.I18nMessage;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ServiceResult {

	private ServiceResultState state;
	private String message;
	private Throwable throwable;

	private I18nMessage i18nMessage;

	public ServiceResult() {
		//
	}

	public ServiceResult(ServiceResultState state) {
		this.state = state;
	}

	public ServiceResult(ServiceResultState state, String message) {
		this.state = state;
		this.message = message;
	}

	public ServiceResult(ServiceResultState state, String message, Throwable throwable) {
		this.state = state;
		this.message = message;
		this.throwable = throwable;
	}

	public ServiceResult(ServiceResultState state, I18nMessage i18nMessage) {
		this.state = state;
		this.message = i18nMessage.getMessage(Locale.getDefault());
		this.i18nMessage = i18nMessage;
	}

	public ServiceResult(ServiceResultState state, String message, I18nMessage i18nMessage) {
		this.state = state;
		this.message = message;
		this.i18nMessage = i18nMessage;
	}

	/**
	 * @return true if the state is {@link ServiceResultState#SUCCESS}
	 */
	public boolean isOk() {
		return this.state == ServiceResultState.SUCCESS;
	}

	/**
	 * @return true if the state is NOT {@link ServiceResultState#SUCCESS}
	 */
	public boolean isNok() {
		return this.state != ServiceResultState.SUCCESS;
	}

	public ServiceResultState getState() {
		return this.state;
	}

	public void setState(ServiceResultState state) {
		this.state = state;
	}

	public String getMessage() {
		return this.message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getRootMessage() {
		Throwable t = getRootCause();
		if (t == null)
			return null;
		return t.getMessage() == null ? t.getClass().getName() : (t.getClass().getName() + ": " + t.getMessage());
	}

	/**
	 * @return the root cause of the encapsulated {@link Throwable} or null if no {@link Throwable} is set
	 */
	public Throwable getRootCause() {
		if (this.throwable == null)
			return null;
		Throwable t = this.throwable;
		while (t.getCause() != null) {
			t = t.getCause();
		}

		return t;
	}

	public Throwable getThrowable() {
		return this.throwable;
	}

	public void setThrowable(Throwable throwable) {
		this.throwable = throwable;
	}

	public static ServiceResult success() {
		return new ServiceResult(ServiceResultState.SUCCESS);
	}

	public static ServiceResult success(String msg) {
		return new ServiceResult(ServiceResultState.SUCCESS, msg);
	}

	public static ServiceResult warning(String warning) {
		return new ServiceResult(ServiceResultState.WARNING, warning);
	}

	public static ServiceResult warning(String warning, Throwable t) {
		return new ServiceResult(ServiceResultState.WARNING, warning, t);
	}

	public static ServiceResult error(String error) {
		return new ServiceResult(ServiceResultState.FAILED, error);
	}

	public static ServiceResult error(I18nMessage i18nMessage) {
		return new ServiceResult(ServiceResultState.FAILED, i18nMessage);
	}

	public static ServiceResult error(String message, I18nMessage i18nMessage) {
		return new ServiceResult(ServiceResultState.FAILED, message, i18nMessage);
	}

	public static ServiceResult failed(String error, Throwable t) {
		return new ServiceResult(ServiceResultState.EXCEPTION, error, t);
	}

	@SuppressWarnings("unchecked")
	public <T extends ServiceResult> T i18n(I18nMessage message) {
		this.i18nMessage = message;
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public <T extends ServiceResult> T i18n(ResourceBundle bundle, String key) {
		this.i18nMessage = new I18nMessage(bundle, key);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public <T extends ServiceResult> T i18n(ResourceBundle bundle, String key, String prop, Object value) {
		this.i18nMessage = new I18nMessage(bundle, key) //
				.value(prop, value);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public <T extends ServiceResult> T i18n(ResourceBundle bundle, String key, String prop1, Object value1,
			String prop2, Object value2) {
		this.i18nMessage = new I18nMessage(bundle, key) //
				.value(prop1, value1) //
				.value(prop2, value2);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public <T extends ServiceResult> T i18n(ResourceBundle bundle, String key, String prop1, Object value1,
			String prop2, Object value2, String prop3, Object value3) {
		this.i18nMessage = new I18nMessage(bundle, key) //
				.value(prop1, value1) //
				.value(prop2, value2) //
				.value(prop3, value3);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public <T extends ServiceResult> T i18n(ResourceBundle bundle, String key, String prop1, Object value1,
			String prop2, Object value2, String prop3, Object value3, String prop4, Object value4) {
		this.i18nMessage = new I18nMessage(bundle, key) //
				.value(prop1, value1) //
				.value(prop2, value2) //
				.value(prop3, value3) //
				.value(prop4, value4);
		return (T) this;
	}

	public I18nMessage getI18nMessage() {
		return this.i18nMessage;
	}

	public void setI18nMessage(I18nMessage i18nMessage) {
		this.i18nMessage = i18nMessage;
	}

	public JsonObject toJson() {
		JsonObject json = new JsonObject();

		json.addProperty(Tags.Json.STATE, this.state.name());
		json.addProperty(MSG, isEmpty(this.message) ? "-" : this.message);

		if (this.throwable != null) {
			json.addProperty(EXCEPTION_MSG, getExceptionMessageWithCauses(this.throwable, false));
			json.addProperty(THROWABLE, formatException(this.throwable));

			if (this.throwable instanceof StrolchException ex && ex.hasI18n())
				json.add(I_18_N, ex.getI18n().accept(new I18nMessageToJsonVisitor()));
		}

		if (!json.has(I_18_N) && this.i18nMessage != null)
			json.add("i18n", this.i18nMessage.accept(new I18nMessageToJsonVisitor()));

		return json;
	}
}
