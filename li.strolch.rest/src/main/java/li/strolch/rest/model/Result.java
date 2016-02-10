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
package li.strolch.rest.model;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import ch.eitchnet.utils.helper.StringHelper;
import li.strolch.service.api.ServiceResult;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "Result")
@XmlAccessorType(XmlAccessType.NONE)
public class Result {

	@XmlAttribute(name = "msg")
	private String msg;

	@XmlAttribute(name = "exceptionMsg")
	private String exceptionMsg;

	public Result(String msg) {
		this.msg = msg;
	}

	public Result(Exception e) {
		this.msg = StringHelper.isEmpty(e.getMessage()) ? e.getClass().getName() : e.getMessage();
		this.exceptionMsg = StringHelper.formatExceptionMessage(e);
	}

	public Result(ServiceResult svcResult) {
		if (svcResult.isOk()) {
			this.msg = StringHelper.DASH;
		} else {
			this.msg = svcResult.getMessage();
			Throwable t = svcResult.getThrowable();
			if (t != null)
				this.exceptionMsg = StringHelper.formatExceptionMessage(t);
		}
	}

	public Result() {
		this.msg = StringHelper.DASH;
	}

	public String getMsg() {
		return this.msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}

	public String getExceptionMsg() {
		return exceptionMsg;
	}

	public void setExceptionMsg(String exceptionMsg) {
		this.exceptionMsg = exceptionMsg;
	}

	public static Response toResponse(ServiceResult svcResult) {
		Result result = new Result(svcResult);
		if (svcResult.isOk())
			return Response.ok(result, MediaType.APPLICATION_JSON).build();
		return Response.serverError().type(MediaType.APPLICATION_JSON).entity(result).build();
	}
}
