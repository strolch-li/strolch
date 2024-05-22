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
package li.strolch.service.privilege.groups;

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.json.PrivilegeElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.handler.PrivilegeHandler;
import li.strolch.privilege.model.Group;
import li.strolch.service.JsonServiceResult;
import li.strolch.service.StringArgument;
import li.strolch.service.api.AbstractService;

import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeRemoveGroupService extends AbstractService<StringArgument, JsonServiceResult> {

	@Override
	protected JsonServiceResult getResultInstance() {
		return new JsonServiceResult();
	}

	@Override
	public StringArgument getArgumentInstance() {
		return new StringArgument();
	}

	@Override
	protected JsonServiceResult internalDoService(StringArgument arg) {
		String groupName = arg.value;

		li.strolch.runtime.privilege.PrivilegeHandler strolchPrivilegeHandler = getContainer().getPrivilegeHandler();
		PrivilegeHandler privilegeHandler = strolchPrivilegeHandler.getPrivilegeHandler();

		Group group;
		try (StrolchTransaction tx = openArgOrUserTx(arg, PRIVILEGE_REMOVE_GROUP)) {
			tx.setSuppressAudits(true);

			group = privilegeHandler.removeGroup(getCertificate(), groupName);
			privilegeHandler.persist(getCertificate());

			Audit audit = tx.auditFrom(AccessType.DELETE, PRIVILEGE, GROUP, groupName);
			tx.getAuditTrail().add(tx, audit);
		}

		return new JsonServiceResult(group.accept(new PrivilegeElementToJsonVisitor()));
	}
}
