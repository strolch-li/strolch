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

package li.strolch.websocket;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.websocket.HandshakeResponse;
import jakarta.websocket.server.HandshakeRequest;
import jakarta.websocket.server.ServerEndpointConfig;
import li.strolch.rest.RestfulStrolchComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.List;

import static li.strolch.rest.StrolchRestfulConstants.HEADER_X_FORWARDED_FOR;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_REMOTE_IP;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

public class WebSocketConfigurator extends ServerEndpointConfig.Configurator {

	private static final Logger logger = LoggerFactory.getLogger(WebSocketConfigurator.class);

	@Override
	public void modifyHandshake(ServerEndpointConfig sec, HandshakeRequest request, HandshakeResponse response) {
		String remoteIp = getRemoteIp(request);
		logger.info("Remote IP: {}: {} {}", remoteIp, "WS Upgrade", request.getRequestURI());
		sec.getUserProperties().put(STROLCH_REMOTE_IP, remoteIp);
	}

	public static String getRemoteIp(HandshakeRequest request) {
		try {
			HttpServletRequest httpSession = (HttpServletRequest) request.getHttpSession();

			String remoteHost;
			String remoteAddr;
			if (httpSession != null) {
				remoteHost = httpSession.getRemoteHost();
				remoteAddr = httpSession.getRemoteAddr();
			} else {
				HttpServletRequest httpServletRequest = getHttpServletRequest(request);
				if (httpServletRequest != null) {
					remoteAddr = httpServletRequest.getRemoteAddr();
					remoteHost = httpServletRequest.getRemoteHost();
				} else {
					remoteHost = "unknown";
					remoteAddr = "unknown";
				}
			}

			List<String> xForwardedForList = request.getHeaders().get(HEADER_X_FORWARDED_FOR);
			String xForwardedFor = xForwardedForList == null || xForwardedForList.isEmpty() ? null :
					xForwardedForList.getFirst();

			StringBuilder sb = new StringBuilder();
			if (isNotEmpty(xForwardedFor) && RestfulStrolchComponent
					.getInstance()
					.getForwardIgnoreIp()
					.equals(remoteAddr)) {
				return xForwardedFor;
			}

			if (remoteHost.equals(remoteAddr))
				sb.append(remoteAddr);
			else {
				sb.append(remoteHost).append(": (").append(remoteAddr).append(")");
			}

			if (isNotEmpty(xForwardedFor))
				sb.append(" (fwd)=> ").append(xForwardedFor);

			return sb.toString();
		} catch (Exception e) {
			logger.error("Failed to get remote IP from request!", e);
			return "unknown";
		}
	}

	private static HttpServletRequest getHttpServletRequest(HandshakeRequest request) {
		try {
			Field f = request.getClass().getDeclaredField("request");
			f.setAccessible(true);
			return (HttpServletRequest) f.get(request);
		} catch (Exception e) {
			logger.error("Failed to get HttpServletRequest from request!", e);
			return null;
		}
	}
}