/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.rest;

import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;
import java.text.MessageFormat;

import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.rest.helper.ResponseUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Provider
public class StrolchRestfulExceptionMapper implements ExceptionMapper<Exception> {

	private static final Logger logger = LoggerFactory.getLogger(StrolchRestfulExceptionMapper.class);

	@Override
	public Response toResponse(Exception ex) {

		logger.error(MessageFormat.format("Handling exception {0}", ex.getClass()), ex); //$NON-NLS-1$

		if (ex instanceof NotFoundException)
			return ResponseUtil.toResponse(Status.NOT_FOUND, ex);

		if (ex instanceof StrolchAccessDeniedException) {
			StrolchAccessDeniedException e = (StrolchAccessDeniedException) ex;
			return ResponseUtil.toResponse(Status.FORBIDDEN, e.getI18n());
		}

		if (ex instanceof StrolchNotAuthenticatedException) {
			StrolchNotAuthenticatedException e = (StrolchNotAuthenticatedException) ex;
			logger.error("User tried to access resource, but was not authenticated: " + ex.getMessage());
			return Response.status(Status.UNAUTHORIZED).entity(e.getMessage()).type(MediaType.TEXT_PLAIN).build();
		}

		return ResponseUtil.toResponse(ex);
	}
}