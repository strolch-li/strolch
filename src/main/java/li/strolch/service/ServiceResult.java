/*
 * Copyright (c) 2012
 *
 * This file is part of ???????????????
 *
 * ?????????????? is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ????????????????.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package li.strolch.service;

import java.io.Serializable;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
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

	public static ServiceResult failed(String error, Throwable t) {
		return new ServiceResult(ServiceResultState.FAILED, error, t);
	}
}
