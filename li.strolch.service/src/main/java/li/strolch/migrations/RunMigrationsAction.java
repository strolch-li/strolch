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
package li.strolch.migrations;

import java.util.Map;

import ch.eitchnet.privilege.handler.SystemUserAction;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.utils.Version;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RunMigrationsAction implements SystemUserAction {

	private Migrations migrations;
	private Map<String, Version> currentVersions;

	/**
	 * @param migrations
	 * @param currentVersions
	 */
	public RunMigrationsAction(Migrations migrations, Map<String, Version> currentVersions) {
		this.migrations = migrations;
		this.currentVersions = currentVersions;
	}

	@Override
	public void execute(PrivilegeContext privilegeContext) {
		this.migrations.runMigrations(privilegeContext.getCertificate(), currentVersions);
	}
}
