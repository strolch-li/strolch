/*
 * Copyright (c) 2012
 *
 * This file is part of ???????????????
 *
 * ?????????????? is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ????????????????.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.privilege.handler;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;

/**
 * With this interface system actions, which are to be performed in an automated fashion, i.e. by cron jobs, can be
 * implemented and then the authorized execution can be delegated to
 * {@link PrivilegeHandler#runAsSystem(String, SystemUserAction)}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface SystemUserAction {

	/**
	 * This method will be called by the {@link PrivilegeHandler} when an authorized {@link Certificate} has been
	 * generated to allow this action to properly validate its execution
	 * 
	 * TODO: I'm not really happy with this... we might want to pass the certificate and then force the action to
	 * validate it to get the {@link PrivilegeContext} - we don't want the {@link PrivilegeContext} to live forever...
	 * 
	 * @param privilegeContext
	 *            the {@link PrivilegeContext} which was generated for a valid system user
	 */
	public void execute(PrivilegeContext privilegeContext);
}
