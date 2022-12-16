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
package li.strolch.privilege.handler;

import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;

/**
 * With this interface system actions, which are to be performed in an automated fashion, i.e. by cron jobs, can be
 * implemented and then the authorized execution can be delegated to {@link PrivilegeHandler#runAs(String,
 * SystemAction)}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class SystemAction implements Restrictable {

	@Override
	public String getPrivilegeName() {
		return SystemAction.class.getName();
	}

	@Override
	public Object getPrivilegeValue() {
		return this.getClass().getName();
	}

	/**
	 * This method will be called by the {@link PrivilegeHandler} when an authorized {@link Certificate} has been
	 * generated to allow this action to properly validate its execution
	 *
	 * @param privilegeContext
	 * 		the {@link PrivilegeContext} which was generated for a valid system user
	 */
	public abstract void execute(PrivilegeContext privilegeContext) throws Exception;
}
