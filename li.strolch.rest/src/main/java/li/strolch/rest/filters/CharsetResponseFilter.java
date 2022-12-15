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

import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerResponseContext;
import jakarta.ws.rs.container.ContainerResponseFilter;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.ext.Provider;

/**
 * The JSON generated is not in the same charset as the rest of the response, thus we override it to UTF-8 with this
 * response filter
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Provider
public class CharsetResponseFilter implements ContainerResponseFilter {

	private static final String UTF_8 = "utf-8"; //$NON-NLS-1$

	@Override
	public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext)
			throws IOException {

		MediaType contentType = responseContext.getMediaType();
		if (contentType != null) {
			String charset = contentType.getParameters().get(MediaType.CHARSET_PARAMETER);
			if (charset == null || !charset.equalsIgnoreCase(UTF_8)) {
				contentType = contentType.withCharset(UTF_8);
				responseContext.getHeaders().putSingle(HttpHeaders.CONTENT_TYPE, contentType.toString());
			}
		}
	}
}