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
import com.google.gson.JsonParser;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.persistence.api.Operation;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.notifications.NotificationsPolicy;
import li.strolch.privilege.model.Certificate;
import li.strolch.rest.RestfulStrolchComponent;
import li.strolch.rest.helper.ResponseUtil;
import li.strolch.service.JsonServiceArgument;
import li.strolch.service.StringArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.notifications.CreateNotificationService;
import li.strolch.service.notifications.RemoveNotificationService;
import li.strolch.service.notifications.UpdateNotificationService;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.rest.RestfulStrolchComponent.getInstance;
import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.STROLCH_CERTIFICATE;
import static li.strolch.rest.helper.ResponseUtil.*;
import static li.strolch.runtime.StrolchConstants.DEFAULT_REALM;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.*;
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

	private static Certificate validateCertificate(HttpServletRequest request, String privilege) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);
		RestfulStrolchComponent rest = RestfulStrolchComponent.getInstance();
		rest.validate(cert).validateAction(privilege, getCallerMethodNoClass(2));
		return cert;
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getUserNotifications(@Context HttpServletRequest request) {
		Certificate cert = validateCertificate(request, PRIVILEGE_GET_NOTIFICATIONS);
		try (StrolchTransaction tx = openTx(cert)) {
			List<Resource> notifications = NotificationsPolicy.getDefaultPolicy(tx).findUserNotifications();
			Function<Resource, JsonObject> visitor = notificationToJson(tx.getAgent(), cert);
			return toResponse(DATA, notifications.stream().map(visitor).filter(Objects::nonNull).toList());
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return toResponse(e);
		}
	}

	@GET
	@Path("{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getNotification(@Context HttpServletRequest request, @PathParam("id") String id) {
		Certificate cert = validateCertificate(request, PRIVILEGE_GET_NOTIFICATION);
		try (StrolchTransaction tx = openTx(cert)) {
			Resource notification = tx.getResourceBy(TYPE_NOTIFICATION, id, true);
			tx.assertHasPrivilege(Operation.GET, notification);
			return toResponse(DATA, notificationToJson(tx.getAgent(), cert).apply(notification));
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return toResponse(e);
		}
	}

	@GET
	@Path("all")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getAllNotifications(@Context HttpServletRequest request) {
		Certificate cert = validateCertificate(request, PRIVILEGE_GET_NOTIFICATIONS_ALL);
		try (StrolchTransaction tx = openTx(cert)) {
			StrolchRootElementToJsonVisitor visitor = new StrolchRootElementToJsonVisitor()
					.withoutPolicies()
					.withoutVersion()
					.withoutStateVariables()
					.flatBagsByType(TYPE_TEXT, TYPE_VISIBILITY)
					.resourceHook((notification, notificationJ) -> addLocationNames(notification, notificationJ, tx));
			return toResponse(DATA, tx.streamResources(TYPE_NOTIFICATION).map(a -> a.accept(visitor)).toList());
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return toResponse(e);
		}
	}

	private static void addLocationNames(Resource notification, JsonObject notificationJ, StrolchTransaction tx) {
		if (!notification.hasParameter(BAG_VISIBILITY, PARAM_LOCATIONS))
			return;
		JsonArray locationNamesJ = notification
				.getStringList(BAG_VISIBILITY, PARAM_LOCATIONS)
				.stream()
				.map(locationId -> {
					Resource location = tx.getResourceBy(TYPE_LOCATION, locationId);
					return location == null ? locationId : location.getName();
				})
				.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);
		notificationJ.get(BAG_VISIBILITY).getAsJsonObject().add(PARAM_LOCATION_NAMES, locationNamesJ);
	}

	private static Function<Resource, JsonObject> notificationToJson(StrolchAgent agent, Certificate cert) {
		return notification -> {
			JsonObject notificationJ = new JsonObject();
			notificationJ.addProperty(Tags.Json.ID, notification.getId());

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

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response create(@Context HttpServletRequest request, String data) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// get JSON
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();

		// create service and argument
		CreateNotificationService svc = new CreateNotificationService();
		ServiceHandler svcHandler = getInstance().getServiceHandler();
		JsonServiceArgument arg = svc.getArgumentInstance();
		arg.jsonElement = jsonObject;

		// call service
		ServiceResult result = svcHandler.doService(cert, svc, arg);
		return toResponse(result);
	}

	@PUT
	@Path("{id}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response update(@Context HttpServletRequest request, @PathParam("id") String id, String data) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// get JSON
		JsonObject jsonObject = JsonParser.parseString(data).getAsJsonObject();

		// create service and argument
		UpdateNotificationService svc = new UpdateNotificationService();
		ServiceHandler svcHandler = getInstance().getServiceHandler();
		JsonServiceArgument arg = svc.getArgumentInstance();
		arg.objectId = id;
		arg.jsonElement = jsonObject;

		// call service
		ServiceResult result = svcHandler.doService(cert, svc, arg);
		return toResponse(result);
	}

	@DELETE
	@Path("{id}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response remove(@Context HttpServletRequest request, @PathParam("id") String id) {
		Certificate cert = (Certificate) request.getAttribute(STROLCH_CERTIFICATE);

		// create service and argument
		RemoveNotificationService svc = new RemoveNotificationService();
		ServiceHandler svcHandler = getInstance().getServiceHandler();
		StringArgument arg = svc.getArgumentInstance();
		arg.value = id;

		// call service
		ServiceResult result = svcHandler.doService(cert, svc, arg);
		return toResponse(result);
	}
}
