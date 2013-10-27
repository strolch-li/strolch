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
package li.strolch.command;

import li.strolch.persistence.api.StrolchTransaction;
import ch.eitchnet.privilege.model.Restrictable;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class Command implements Restrictable {

	private final StrolchTransaction tx;

	public Command(StrolchTransaction tx) {
		this.tx = tx;
	}

	/**
	 * Returns the {@link StrolchTransaction} bound to this {@link Command}'s runtime
	 * 
	 * @return the {@link StrolchTransaction} bound to this {@link Command}'s runtime
	 */
	protected StrolchTransaction tx() {
		return this.tx;
	}

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeName()
	 */
	@Override
	public String getPrivilegeName() {
		return Command.class.getName();
	}

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeValue()
	 */
	@Override
	public Object getPrivilegeValue() {
		return this.getClass().getName();
	}

	public abstract void doCommand();
}
