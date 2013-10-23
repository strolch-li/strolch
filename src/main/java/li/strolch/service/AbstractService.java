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
package ch.eitchnet.service;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class AbstractService implements Service {
	private static final long serialVersionUID = 1L;

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeName()
	 */
	@Override
	public String getPrivilegeName() {
		return Service.class.getName();
	}

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeValue()
	 */
	@Override
	public Object getPrivilegeValue() {
		return this.getClass().getName();
	}
}
