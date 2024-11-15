/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.privilege.helper;

import li.strolch.privilege.handler.WindowsLdapQueryContext;

import javax.naming.NamingException;
import javax.naming.directory.Attributes;
import java.util.Map;

public class LinuxLdapQueryContext extends WindowsLdapQueryContext {

	public LinuxLdapQueryContext(Map<String, String> parameterMap, RemoteGroupMappingModel groupMappingModel) {
		super(parameterMap, groupMappingModel);
	}

	@Override
	public String getDistinguishedName(String safeUsername) {
		return "uid=" + safeUsername + "," + this.searchBase;
	}

	@Override
	public String validateLdapUsername(String username, Attributes attrs) throws NamingException {
		return super.validateLdapUsername(username, attrs);
	}

	@Override
	public String getUserAttributeIdentifier1() {
		return "uid";
	}

	@Override
	public String getObjectClassFilter() {
		return "(objectClass=person)";
	}
}
