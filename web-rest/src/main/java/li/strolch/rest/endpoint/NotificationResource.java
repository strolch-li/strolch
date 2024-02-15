/*
 * Copyright 2024 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.rest.endpoint;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.notifications.NotificationsPolicy;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

import static java.util.Optional.ofNullable;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.privilege.base.PrivilegeConstants.REALM;
import static li.strolch.rest.StrolchRestfulConstants.*;
import static li.strolch.runtime.StrolchConstants.DEFAULT_REALM;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.PRIVILEGE_GET_NOTIFICATIONS;
import static li.strolch.utils.helper.ExceptionHelper.getCallerMethod;
import static li.strolch.utils.helper.ExceptionHelper.getCallerMethodNoClass;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Path("strolch/notifications")
public class NotificationResource {

	private static final Logger logger = LoggerFactory.getLogger(NotificationResource.class);

	private StrolchTransaction openTx(Certificate certificate) {
		String realm = certificate.getRealm();
		if (StringHelper.isEmpty(realm))
			realm = DEFAULT_REALM;
		return RestfulStrolchComponent.getInstance().openTx(certificate, realm, getCallerMethod());
	}

	private static Certificate validateCertificate(HttpServletRequest request) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();
		rest.validate(cert).validateAction(PRIVILEGE_GET_NOTIFICATIONS, getCallerMethodNoClass(2));
		return cert;
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getNotifications(@Context HttpServletRequest request) {
		Certificate cert = validateCertificate(request);
		try (StrolchTransaction tx = openTx(cert)) {
			List<Resource> notifications = NotificationsPolicy.getDefaultPolicy(tx).findUserNotifications();
			Function<Resource, JsonObject> visitor = notificationToJson(tx.getAgent(), cert);
			return ResponseUtil.toResponse(DATA, notifications.stream().map(visitor).filter(Objects::nonNull).toList());
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return ResponseUtil.toResponse(e);
		}
	}

	private static Function<Resource, JsonObject> notificationToJson(StrolchAgent agent, Certificate cert) {
		return notification -> {
			JsonObject notificationJ = new JsonObject();

			String lang = cert.getLocale().getLanguage();
			Optional<ParameterBag> textBagO = ofNullable(notification.getParameterBag(lang))
					.or(() -> ofNullable(notification.getParameterBag(agent.getLocale().getLanguage())))
					.or(() -> notification.streamOfParameterBagsByType(TYPE_TEXT).findFirst());
			if (textBagO.isEmpty())
				return null;
			ParameterBag textBag = textBagO.get();

			notificationJ.addProperty(PARAM_TITLE, textBag.getString(PARAM_TITLE));
			notificationJ.addProperty(PARAM_TEXT, textBag.getString(PARAM_TEXT));
			notificationJ.addProperty(PARAM_VISIBLE_FROM,
					ISO8601.toString(notification.getDate(BAG_VISIBILITY, PARAM_VISIBLE_FROM)));
			notificationJ.addProperty(PARAM_VISIBLE_TO,
					ISO8601.toString(notification.getDate(BAG_VISIBILITY, PARAM_VISIBLE_TO)));

			return notificationJ;
		};
	}
}
