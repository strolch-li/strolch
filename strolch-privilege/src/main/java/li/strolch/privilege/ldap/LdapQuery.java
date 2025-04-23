/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.privilege.ldap;

import javax.naming.NamingException;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;

public abstract class LdapQuery implements AutoCloseable {

	protected final SearchControls searchControls;

	protected LdapQuery() {
		this.searchControls = new SearchControls();
		this.searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE	);
		this.searchControls.setReturningAttributes(new String[]{"*", "+"});
	}

	public abstract SearchResult searchLdap(String username, char[] password) throws NamingException;

	@Override
	public abstract void close();
}
