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
package li.strolch.service.privilege.users;

import java.util.HashSet;
import java.util.Set;

import com.google.gson.JsonArray;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.UserRep;
import li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants;
import li.strolch.service.JsonServiceArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResultState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeUpdateUserRolesService extends AbstractService<JsonServiceArgument, PrivilegeUserResult> {

	@Override
	protected PrivilegeUserResult getResultInstance() {
		return new PrivilegeUserResult(ServiceResultState.FAILED);
	}

	@Override
	public JsonServiceArgument getArgumentInstance() {
		return new JsonServiceArgument();
	}

	@Override
	protected PrivilegeUserResult internalDoService(JsonServiceArgument arg) {

		li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler = getContainer().getPrivilegeHandler();
		PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler();

		JsonArray rolesE = arg.jsonElement.getAsJsonArray();
		Set<String> roles = new HashSet<>();
		rolesE.forEach(e -> roles.add(e.getAsString()));

		String username = arg.objectId;

		UserRep user;
		try (StrolchTransaction tx = openArgOrUserTx(arg, PrivilegeHandler.PRIVILEGE_ADD_ROLE_TO_USER)) {
			tx.setSuppressAudits(true);

			user = privilegeHandler.getUser(getCertificate(), username);

			// first add new roles
			boolean changed = false;
			for (String role : roles) {
				if (!user.hasRole(role)) {
					user = privilegeHandler.addRoleToUser(getCertificate(), username, role);
					changed = true;
				}
			}

			// handle removed roles
			for (String role : user.getRoles()) {
				if (!roles.contains(role)) {
					user = privilegeHandler.removeRoleFromUser(getCertificate(), username, role);
					changed = true;
				}
			}

			if (changed) {

				if (privilegeHandler.isPersistOnUserDataChanged())
					privilegeHandler.persist(getCertificate());

				Audit audit = tx.auditFrom(AccessType.UPDATE, StrolchPrivilegeConstants.PRIVILEGE,
						StrolchPrivilegeConstants.USER, user.getUsername());
				tx.getAuditTrail().add(tx, audit);
			}
		}

		return new PrivilegeUserResult(user);
	}
}
