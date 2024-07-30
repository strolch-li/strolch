package li.strolch.websocket;

import static li.strolch.model.StrolchModelConstants.ROLE_STROLCH_ADMIN;
import static li.strolch.model.Tags.Json.*;
import static li.strolch.rest.StrolchRestfulConstants.MSG;
import static li.strolch.runtime.StrolchConstants.DEFAULT_REALM;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessage;
import static li.strolch.utils.helper.StringHelper.*;

import jakarta.websocket.CloseReason;
import jakarta.websocket.EndpointConfig;
import jakarta.websocket.MessageHandler;
import jakarta.websocket.Session;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.agent.api.ObserverHandler;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.exception.StrolchNotAuthenticatedException;
import li.strolch.model.Tags;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.sessions.StrolchSessionHandler;
import li.strolch.utils.helper.ExceptionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WebSocketClient implements MessageHandler.Whole<String> {

	public static final Logger logger = LoggerFactory.getLogger(WebSocketClient.class);

	protected final StrolchSessionHandler sessionHandler;
	protected final StrolchAgent agent;
	protected final Session session;
	protected final EndpointConfig config;
	protected final String remoteIp;
	protected final Map<String, WebSocketObserverHandler> observerHandlersByRealm;

	protected Certificate certificate;

	public WebSocketClient(StrolchAgent agent, Session session, EndpointConfig config) {
		this.agent = agent;
		this.sessionHandler = agent.getComponent(StrolchSessionHandler.class);
		this.session = session;
		this.config = config;
		this.remoteIp = WebSocketRemoteIp.get();
		this.observerHandlersByRealm = new HashMap<>(1);
	}

	@Override
	public String toString() {
		return "WebSocket " + this.certificate.getUsername() + "@" + this.remoteIp;
	}

	@Override
	public void onMessage(String message) {

		JsonObject jsonObject = JsonParser.parseString(message).getAsJsonObject();
		String msgType = jsonObject.get(MSG_TYPE).getAsString();
		logger.info("Handling message {}", msgType);

		switch (msgType) {
		case "Authenticate" -> handleAuthenticate(jsonObject);
		case "ObserverRegister" -> {
			assertAuthenticated(msgType);
			handleRegister(jsonObject);
		}
		case "ObserverUnregister" -> {
			assertAuthenticated(msgType);
			handleUnregister(jsonObject);
		}
		default -> logger.error("Unhandled Event msgType: {}", msgType);
		}
	}

	public void assertAuthenticated(String type) {
		if (this.certificate == null) {
			logger.error("Received {} request, but not yet authed!", type);
			close(CloseReason.CloseCodes.PROTOCOL_ERROR, "Not yet authed!");
			return;
		}

		try {
			this.sessionHandler.validate(this.certificate, this.remoteIp);
		} catch (RuntimeException e) {
			logger.error("Received {} request, but authentication is not valid anymore: {}", type,
					ExceptionHelper.getExceptionMessage(e));
			close(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Not yet authed!");
		}
	}

	private void handleRegister(JsonObject jsonObject) {
		String realmS;
		if (jsonObject.has(REALM) && isNotEmpty(jsonObject.get(REALM).getAsString())) {
			realmS = jsonObject.get(Tags.Json.REALM).getAsString();
			if (isEmpty(this.certificate.getRealm()) && !this.certificate.hasRole(ROLE_STROLCH_ADMIN))
				throw new IllegalStateException("User is missing realm configuration!");

			if (!this.certificate.hasRole(ROLE_STROLCH_ADMIN) && !realmS.equals(this.certificate.getRealm())) {
				throw new IllegalStateException("User does not have access to realm " + realmS);
			}
		} else if (isNotEmpty(this.certificate.getRealm())) {
			realmS = this.certificate.getRealm();
		} else {
			realmS = DEFAULT_REALM;
		}

		String realm = realmS;

		String objectType = jsonObject.get(Tags.Json.OBJECT_TYPE).getAsString();
		String type = jsonObject.get(Tags.Json.TYPE).getAsString();

		JsonObject params = jsonObject.get(PARAMS).getAsJsonObject();

		this.observerHandlersByRealm.computeIfAbsent(objectType, s -> {
			ObserverHandler observerHandler = this.agent.getRealm(realm).getObserverHandler();
			return getWebSocketObserverHandler(realm, observerHandler);
		}).register(objectType, type, params);

		String username = this.certificate.getUsername();
		logger.info("{} registered for {} {} params: {}", username, objectType, type, params);
	}

	protected WebSocketObserverHandler getWebSocketObserverHandler(String realm, ObserverHandler observerHandler) {
		return new WebSocketObserverHandler(this.agent, realm, observerHandler, this);
	}

	private void handleUnregister(JsonObject jsonObject) {
		String realm = jsonObject.get(Tags.Json.REALM).getAsString();
		String objectType = jsonObject.get(Tags.Json.OBJECT_TYPE).getAsString();
		String type = jsonObject.get(Tags.Json.TYPE).getAsString();
		WebSocketObserverHandler webSocketObserverHandler = this.observerHandlersByRealm.get(realm);
		if (webSocketObserverHandler == null) {
			logger.error("Client {} for {} not registered for {} {} {}", this.session.getId(),
					this.certificate.getUsername(), realm, objectType, type);
		} else {
			webSocketObserverHandler.unregister(objectType, type);
		}
	}

	private void handleAuthenticate(JsonObject jsonObject) {
		if (!jsonObject.has("authToken") || jsonObject.get("authToken").isJsonNull() || !jsonObject.has("username")
				|| jsonObject.get("username").isJsonNull()) {
			logger.error("Received invalid authentication request: {}", jsonObject);
			close(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Invalid authentication");
			return;
		}

		String authToken = jsonObject.get("authToken").getAsString();
		String username = jsonObject.get("username").getAsString();

		if (authToken.isEmpty() || username.isEmpty()) {
			logger.error("Received invalid authentication request: {}", jsonObject);
			close(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Invalid authentication");
			return;
		}

		try {
			this.certificate = this.sessionHandler.validate(authToken, this.remoteIp);
			if (!this.certificate.getUsername().equals(username)) {
				logger.error("Invalid authentication for {}", username);
				close(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Invalid authentication");
				return;
			}
			logger.info("User {} authenticated on WebSocket with remote IP {}", this.certificate.getUsername(),
					this.remoteIp);
		} catch (StrolchNotAuthenticatedException e) {
			logger.error("Failed to authenticate user {}: {}", username, e.getMessage());
			close(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Invalid authentication");
			return;
		} catch (Exception e) {
			logger.error("Failed to authenticate user {}", username, e);
			close(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Invalid authentication");
			return;
		}

		jsonObject.addProperty(MSG, DASH);

		try {
			sendMessage(jsonObject.toString());
		} catch (Exception e) {
			logger.error("Failed to send authentication response to client {}", this, e);
			close(new CloseReason(CloseReason.CloseCodes.CLOSED_ABNORMALLY, getExceptionMessage(e)));
		}
	}

	private void close(CloseReason.CloseCode code, String reason) {
		close(new CloseReason(code, reason));
	}

	public void close(CloseReason closeReason) {
		if (this.observerHandlersByRealm == null || this.observerHandlersByRealm.isEmpty())
			return;
		try {
			this.session.close(new CloseReason(closeReason.getCloseCode(), closeReason.getReasonPhrase()));
		} catch (IOException e) {
			logger.error("Failed to close client after invalid authentication!", e);
		}
		this.observerHandlersByRealm.keySet().forEach(realm -> this.observerHandlersByRealm.get(realm).unregisterAll());
	}

	public void onError(Throwable t) {
		logger.error("Socket error: {}", t.getMessage(), t);
	}

	public synchronized void sendMessage(String data) throws Exception {
		assertAuthenticated("sendMessage");
		this.session.getBasicRemote().sendText(data);
	}
}
