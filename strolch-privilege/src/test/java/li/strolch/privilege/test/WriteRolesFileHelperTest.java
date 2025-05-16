/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.privilege.test;

import li.strolch.privilege.helper.WriteRolesFileHelper;
import org.junit.Test;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;

import static li.strolch.privilege.test.XmlTest.SRC_TEST;
import static org.junit.Assert.assertTrue;

public class WriteRolesFileHelperTest {

	@Test
	public void shouldReadAndWriteRolesFile() throws XMLStreamException, IOException {

		String src = SRC_TEST + "PrivilegeRoles.xml";
		String dst = "target/WriteRolesFileHelperTest_roles.xml";

		if (new File(dst).exists() && !new File(dst).delete())
			throw new IllegalStateException("Could not delete file " + dst);

		WriteRolesFileHelper.main(new String[]{src, dst});

		assertTrue(new File(dst).exists());
	}
}
