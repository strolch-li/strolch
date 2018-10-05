/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.privilege.policy;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;

/**
 * <p>
 * {@link PrivilegePolicy} implements logic to determine if a {@link User} which has the given {@link Role} and the
 * given {@link IPrivilege} has access to the given {@link Restrictable}
 * </p>
 *
 * <p>
 * TODO
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PrivilegePolicy {

	/**
	 * Checks if the given {@link Role} and the given {@link IPrivilege} has access to the given {@link Restrictable}
	 *
	 * @param context
	 * 		the privilege context
	 * @param privilege
	 * 		the {@link IPrivilege} containing the permissions
	 * @param restrictable
	 * 		the {@link Restrictable} to which the user wants access
	 *
	 * @throws AccessDeniedException
	 * 		if action not allowed
	 */
	public void validateAction(PrivilegeContext context, IPrivilege privilege, Restrictable restrictable)
			throws AccessDeniedException;
}
