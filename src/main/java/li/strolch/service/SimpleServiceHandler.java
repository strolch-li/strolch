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
package li.strolch.service;

import java.text.MessageFormat;

import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SimpleServiceHandler {

	private PrivilegeHandler privilegeHandler;

	public ServiceResult doService(Certificate certificate, Service service, ServiceArgument argument) {

		try {

			this.privilegeHandler.getPrivilegeContext(certificate).validateAction(service);

			return service.doService(argument);

		} catch (Exception e) {
			String msg = "Failed to perform service {0} due to exception {1}"; //$NON-NLS-1$
			String error = MessageFormat.format(msg, service.getClass().getName(), e.getMessage());
			return ServiceResult.failed(error, e);
		}
	}
}
