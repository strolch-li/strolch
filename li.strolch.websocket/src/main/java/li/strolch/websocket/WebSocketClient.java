package li.strolch.websocket;

import static li.strolch.model.Tags.Json.*;
import static li.strolch.rest.StrolchRestfulConstants.MSG;
import static li.strolch.runtime.StrolchConstants.DEFAULT_REALM;
import static li.strolch.runtime.StrolchConstants.ROLE_STROLCH_ADMIN;
import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessage;
import static li.strolch.utils.helper.StringHelper.*;

import javax.websocket.CloseReason;
import javax.websocket.EndpointConfig;
import javax.websocket.MessageHandler;
import javax.websocket.Session;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ObserverHandler;
import li.strolch.model.Tags;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.StrolchSessionHandler;
import li.strolch.utils.helper.ExceptionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WebSocketClient implements MessageHandler.Whole<String> {

	public static final Logger logger = LoggerFactory.getLogger(WebSocketClient.class);

	private StrolchSessionHandler sessionHandler;
	private ComponentContainer container;
	private final Session session;
	private final EndpointConfig config;
	private String remoteIp;

	private Certificate certificate;
	private Map<String, WebSocketObserverHandler> observerHandlersByRealm;

	public WebSocketClient(ComponentContainer container, Session session, EndpointConfig config) {
		this.container = container;
		this.sessionHandler = container.getComponent(StrolchSessionHandler.class);
		this.session = session;
		this.config = config;
		this.remoteIp = WebSocketRemoteIp.get();
		this.observerHandlersByRealm = new HashMap<>(1);
	}

	@Override
	public void onMessage(String message) {

		JsonObject jsonObject = new JsonParser().parse(message).getAsJsonObject();
		String msgType = jsonObject.get(MSG_TYPE).getAsString();
		logger.info("Handling message " + msgType);

		switch (msgType) {

		case "Authenticate":
			handleAuthenticate(jsonObject);
			break;

		case "ObserverRegister":
			assertAuthenticated(msgType);
			handleRegister(jsonObject);
			break;
		case "ObserverUnregister":
			assertAuthenticated(msgType);
			handleUnregister(jsonObject);
			break;

		default:
			logger.error("Unhandled Event msgType: " + msgType);
		}
	}

	public void assertAuthenticated(String type) {

		if (this.certificate == null) {
			logger.error("Received " + type + " request, but not yet authed!");
			close(CloseReason.CloseCodes.PROTOCOL_ERROR, "Not yet authed!");
			return;
		}

		try {
			this.sessionHandler.validate(this.certificate, this.remoteIp);
		} catch (RuntimeException e) {
			logger.error("Received " + type + " request, but authentication is not valid anymore: " + ExceptionHelper
					.getExceptionMessage(e));
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
		boolean flat = jsonObject.has(FLAT) && jsonObject.get(FLAT).getAsBoolean();

		this.observerHandlersByRealm.computeIfAbsent(objectType, s -> {
			ObserverHandler observerHandler = this.container.getRealm(realm).getObserverHandler();
			return getWebSocketObserverHandler(observerHandler);
		}).register(objectType, type, flat);
	}

	protected WebSocketObserverHandler getWebSocketObserverHandler(ObserverHandler observerHandler) {
		return new WebSocketObserverHandler(observerHandler, this);
	}

	private void handleUnregister(JsonObject jsonObject) {
		String realm = jsonObject.get(Tags.Json.REALM).getAsString();
		String objectType = jsonObject.get(Tags.Json.OBJECT_TYPE).getAsString();
		String type = jsonObject.get(Tags.Json.TYPE).getAsString();
		WebSocketObserverHandler webSocketObserverHandler = this.observerHandlersByRealm.get(realm);
		if (webSocketObserverHandler == null) {
			logger.error(
					"Client " + this.session.getId() + " for " + this.certificate.getUsername() + " not registered for "
							+ realm + " " + objectType + " " + type);
		} else {
			webSocketObserverHandler.unregister(objectType, type);
		}
	}

	private void handleAuthenticate(JsonObject jsonObject) {

		if (!jsonObject.has("authToken") || !jsonObject.has("username")) {
			logger.error("Received invalid authentication request: " + jsonObject.toString());
			close(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Invalid authentication");
			return;
		}

		String authToken = jsonObject.get("authToken").getAsString();
		String username = jsonObject.get("username").getAsString();

		try {
			this.certificate = this.sessionHandler.validate(authToken, this.remoteIp);
			if (!this.certificate.getUsername().equals(username)) {
				logger.error("Invalid authentication for " + username);
				close(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Invalid authentication");
				return;
			}
		} catch (Exception e) {
			logger.error("Failed to authenticate user " + username, e);
			close(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Invalid authentication");
			return;
		}

		jsonObject.addProperty(MSG, DASH);

		sendMessage(jsonObject.toString());
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
		this.observerHandlersByRealm.keySet().forEach(realm -> {
			this.observerHandlersByRealm.get(realm).unregisterAll();
		});
	}

	public void onError(Throwable t) {
		logger.error("Socket error: " + t.getMessage(), t);
	}

	public synchronized void sendMessage(String data) {
		assertAuthenticated("sendMessage");

		try {
			this.session.getBasicRemote().sendText(data);
		} catch (Exception e) {
			logger.error("Failed to communicate with client", e);
			try {
				this.session.close(new CloseReason(CloseReason.CloseCodes.CLOSED_ABNORMALLY, getExceptionMessage(e)));
			} catch (Exception e1) {
				logger.error("Failed to close", e1);
			}
		}
	}
}
