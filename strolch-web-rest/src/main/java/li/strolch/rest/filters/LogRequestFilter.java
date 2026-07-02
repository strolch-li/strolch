/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.rest.filters;

import jakarta.annotation.Priority;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.container.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.Provider;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.RestfulHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Enumeration;

import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.rest.helper.ServletRequestHelper.logRequest;

@Provider
@PreMatching
@Priority(0)
public class LogRequestFilter implements ContainerRequestFilter, ContainerResponseFilter {

	private static final Logger logger = LoggerFactory.getLogger(LogRequestFilter.class);

	@Context
	private HttpServletRequest request;

	@Override
	public void filter(ContainerRequestContext requestContext) throws IOException {
		String remoteIp = RestfulHelper.getRemoteIp(this.request);
		if (RestfulStrolchComponent.getInstance().isRestLogging())
			logger.info("Remote IP: {}: {} {}", remoteIp, requestContext.getMethod(),
					requestContext.getUriInfo().getRequestUri());

		this.request.setAttribute(STROLCH_REMOTE_IP, remoteIp);
		this.request.setAttribute(STROLCH_REQUEST_URL,
				requestContext.getMethod() + " " + requestContext.getUriInfo().getRequestUri());

		logRequest(this.request);
	}

	@Override
	public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext)
			throws IOException {
		int status = responseContext.getStatus();
		if (status != Response.Status.OK.getStatusCode()) {
			String method = requestContext.getMethod();
			String uri = request.getRequestURI();
			Enumeration<String> contentTypeEnumeration = request.getHeaders("content-type");
			StringBuilder contentType = new StringBuilder();
			while (contentTypeEnumeration.hasMoreElements()) {
				contentType.append(contentTypeEnumeration.nextElement());
				if (contentTypeEnumeration.hasMoreElements())
					contentType.append(", ");
			}

			Enumeration<String> contentEncodingEnumeration = request.getHeaders("content-encoding");
			StringBuilder contentEncoding = new StringBuilder();
			while (contentEncodingEnumeration.hasMoreElements()) {
				contentEncoding.append(contentEncodingEnumeration.nextElement());
				if (contentEncodingEnumeration.hasMoreElements())
					contentEncoding.append(", ");
			}

			if (!responseContext.getHeaders().containsKey(STROLCH_EXCEPTION_I18N))
				logger.error("Request failed {} {}: {} {} Content-type: {}, Content-encoding: {}",
						this.request.getRemoteAddr(), responseContext.getStatus(), method, uri, contentType,
						contentEncoding);
		}
	}
}
	