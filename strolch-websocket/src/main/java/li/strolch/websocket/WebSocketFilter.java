/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.websocket;

import jakarta.servlet.*;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

import static li.strolch.rest.helper.RestfulHelper.getRemoteIp;

@WebFilter("/websocket/*")
public class WebSocketFilter implements Filter {

	private static final Logger logger = LoggerFactory.getLogger(WebSocketFilter.class);

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {

		HttpServletRequest httpRequest = (HttpServletRequest) request;
		String remoteIp = getRemoteIp(httpRequest);
		logger.info("Remote IP: {}: {} {}", remoteIp, httpRequest.getMethod(), httpRequest.getRequestURI());
		WebSocketRemoteIp.set(remoteIp);
		chain.doFilter(request, response);
	}

	public void init(FilterConfig config) {
	}

	public void destroy() {
	}
}