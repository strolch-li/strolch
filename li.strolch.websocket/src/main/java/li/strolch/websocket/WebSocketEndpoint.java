package li.strolch.websocket;

import javax.websocket.*;
import javax.websocket.server.ServerEndpoint;
import java.util.concurrent.ConcurrentHashMap;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.rest.RestfulStrolchComponent;

@ServerEndpoint("/websocket/strolch/observer")
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