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
package li.strolch.rest.filters;

import java.text.MessageFormat;

import jakarta.annotation.Priority;
import jakarta.ws.rs.Priorities;
import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerResponseContext;
import jakarta.ws.rs.container.ContainerResponseFilter;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.ext.Provider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * See <a
 * href="https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/HTTP_Headers_Cheat_Sheet.md">HTTP_Headers_Cheat_Shee</a>
 */
@Provider
@Priority(Priorities.HEADER_DECORATOR)
public class AccessControlResponseFilter implements ContainerResponseFilter {

	private static final String ACCESS_CONTROL_ALLOW_CREDENTIALS = "Access-Control-Allow-Credentials";
	private static final String ACCESS_CONTROL_ALLOW_METHODS = "Access-Control-Allow-Methods";
	private static final String ACCESS_CONTROL_EXPOSE_HEADERS = "Access-Control-Expose-Headers";
	private static final String ACCESS_CONTROL_ALLOW_HEADERS = "Access-Control-Allow-Headers";
	private static final String ACCESS_CONTROL_ALLOW_ORIGIN = "Access-Control-Allow-Origin";
	private static final String X_FRAME_OPTIONS = "X-Frame-Options";
	private static final String X_XSS_PROTECTION = "X-XSS-Protection";
	private static final String X_CONTENT_TYPE_OPTIONS = "X-Content-Type-Options";

	private static final Logger logger = LoggerFactory.getLogger(AccessControlResponseFilter.class);

	private static boolean corsEnabled;
	private static String origin;
	private static boolean logged;

	/**
	 * @param corsEnabled
	 * 		the corsEnabled to set
	 */
	public static void setCorsEnabled(boolean corsEnabled) {
		AccessControlResponseFilter.corsEnabled = corsEnabled;
	}

	/**
	 * @param origin
	 * 		the origin to set
	 */
	public static void setOrigin(String origin) {
		AccessControlResponseFilter.origin = origin;
	}

	@Override
	public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext) {

		if (!corsEnabled)
			return;

		if (!logged) {
			logged = true;
			logger.info("Enabling CORS for origin: {}", origin);
		}

		MultivaluedMap<String, Object> headers = responseContext.getHeaders();

		// Security Headers
		headers.add(X_FRAME_OPTIONS, "DENY");
		headers.add(X_XSS_PROTECTION, 0);
		headers.add(X_CONTENT_TYPE_OPTIONS, "nosniff");

		// allow for the configured origin
		headers.add(ACCESS_CONTROL_ALLOW_ORIGIN, origin);

		// and set the allowed HTTP headers and methods
		headers.add(ACCESS_CONTROL_ALLOW_HEADERS, "Authorization, Origin, X-Requested-With, Content-Type");
		headers.add(ACCESS_CONTROL_EXPOSE_HEADERS, "Authorization, Location, Content-Disposition");
		headers.add(ACCESS_CONTROL_ALLOW_METHODS, "POST, PUT, GET, DELETE, HEAD, OPTIONS");
		headers.add(ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");
	}
}