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

import java.io.Serializable;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ServiceResult implements Serializable {
	private static final long serialVersionUID = 1L;
	private ServiceResultState state;
	private String message;
	private Throwable throwable;

	public ServiceResult() {
		//
	}

	/**
	 * @param state
	 */
	public ServiceResult(ServiceResultState state) {
		this.state = state;
	}

	/**
	 * @param state
	 * @param message
	 * @param throwable
	 */
	public ServiceResult(ServiceResultState state, String message, Throwable throwable) {
		this.state = state;
		this.message = message;
		this.throwable = throwable;
	}

	/**
	 * @return true if the state is {@link ServiceResultState#SUCCESS}
	 */
	public boolean isOk() {
		return this.state == ServiceResultState.SUCCESS;
	}

	/**
	 * @return the state
	 */
	public ServiceResultState getState() {
		return this.state;
	}

	/**
	 * @param state
	 *            the state to set
	 */
	public void setState(ServiceResultState state) {
		this.state = state;
	}

	/**
	 * @return the message
	 */
	public String getMessage() {
		return this.message;
	}

	/**
	 * @param message
	 *            the message to set
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * @return the throwable
	 */
	public Throwable getThrowable() {
		return this.throwable;
	}

	/**
	 * @param throwable
	 *            the throwable to set
	 */
	public void setThrowable(Throwable throwable) {
		this.throwable = throwable;
	}

	public static ServiceResult success() {
		return new ServiceResult(ServiceResultState.SUCCESS, null, null);
	}

	public static ServiceResult success(String msg) {
		return new ServiceResult(ServiceResultState.SUCCESS, msg, null);
	}

	public static ServiceResult warning(String warning) {
		return new ServiceResult(ServiceResultState.WARNING, warning, null);
	}

	public static ServiceResult warning(String warning, Throwable t) {
		return new ServiceResult(ServiceResultState.WARNING, warning, t);
	}

	public static ServiceResult error(String error) {
		return new ServiceResult(ServiceResultState.FAILED, error, null);
	}

	public static ServiceResult failed(String error, Throwable t) {
		return new ServiceResult(ServiceResultState.FAILED, error, t);
	}
}
