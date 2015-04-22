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

import java.io.IOException;

import javax.annotation.Priority;
import javax.ws.rs.Priorities;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.Provider;

@Provider
@Priority(Priorities.HEADER_DECORATOR)
public class HttpCacheResponseFilter implements ContainerResponseFilter {

	public static final String NO_CACHE = "no-cache"; //$NON-NLS-1$

	private static String cacheMode = NO_CACHE;
	
	@Override
	public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext)
			throws IOException {

		MultivaluedMap<String, Object> headers = responseContext.getHeaders();
		headers.add(HttpHeaders.CACHE_CONTROL, cacheMode);
	}

	/**
	 * @return the cacheMode
	 */
	public static String getCacheMode() {
		return cacheMode;
	}

	/**
	 * @param cacheMode
	 *            the cacheMode to set
	 */
	public static void setCacheMode(String cacheMode) {
		HttpCacheResponseFilter.cacheMode = cacheMode;
	}
}