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
package ch.eitchnet.privilege.handler;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.Restrictable;

/**
 * With this interface system actions, which are to be performed in an automated fashion, i.e. by cron jobs, can be
 * implemented and then the authorized execution can be delegated to
 * {@link PrivilegeHandler#runAsSystem(String, SystemUserAction)}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class SystemUserAction implements Restrictable {

	@Override
	public String getPrivilegeName() {
		return SystemUserAction.class.getName();
	}

	@Override
	public Object getPrivilegeValue() {
		return this.getClass().getName();
	}

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
	public abstract void execute(PrivilegeContext privilegeContext);
}
