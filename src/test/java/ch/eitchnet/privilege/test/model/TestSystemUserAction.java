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
package ch.eitchnet.privilege.test.model;

import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.handler.SystemUserAction;
import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
public class TestSystemUserAction implements SystemUserAction {

	private PrivilegeHandler handler;

	/**
	 * 
	 */
	public TestSystemUserAction(PrivilegeHandler handler) {
		this.handler = handler;
	}
	
	/**
	 * @see ch.eitchnet.privilege.handler.SystemUserAction#execute(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public void execute(Certificate certificate) {
		
		TestSystemRestrictable restrictable = new TestSystemRestrictable();
		
		handler.actionAllowed(certificate, restrictable);
	}

}
