/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import jakarta.websocket.*;
import jakarta.websocket.server.ServerEndpoint;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.rest.RestfulStrolchComponent;

import java.util.concurrent.ConcurrentHashMap;

@ServerEndpoint(value = "/websocket/strolch/observer", configurator = WebSocketConfigurator.class)
public class WebSocketEndpoint {

	private final ConcurrentHashMap<Session, WebSocketClient> clientMap = new ConcurrentHashMap<>();

	@OnOpen
	public void onOpen(Session session, EndpointConfig config) {
		StrolchAgent agent = RestfulStrolchComponent.getInstance().getAgent();
		WebSocketClient webSocketClient = new WebSocketClient(agent, session, config);
		this.clientMap.put(session, webSocketClient);
		session.addMessageHandler(webSocketClient);
	}

	@OnClose
	public void onClose(Session session, CloseReason closeReason) {
		WebSocketClient webSocketClient = this.clientMap.remove(session);
		if (webSocketClient != null)
			webSocketClient.close(closeReason);
	}

	@OnError
	public void onError(Session session, Throwable t) {
		WebSocketClient webSocketClient = this.clientMap.get(session);
		if (webSocketClient != null)
			webSocketClient.onError(t);
	}
}