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
package li.strolch.runtime.query.enums;

import static org.junit.Assert.assertEquals;

import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.junit.Test;

import li.strolch.RuntimeMock;
import li.strolch.agent.ComponentContainerTest;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class EnumHandlerTest {

	private static final String ENUM_HANDLER_TEST_RUNTIME = "target/EnumHandlerTest/";

	@Test
	public void shouldFindByLocator() {

		RuntimeMock.runInStrolch(ENUM_HANDLER_TEST_RUNTIME, ComponentContainerTest.PATH_TRANSIENT_CONTAINER, agent -> {

			ComponentContainer container = agent.getContainer();

			Certificate certificate = container.getPrivilegeHandler().authenticate("test", "test".toCharArray());

			EnumHandler enumHandler = container.getComponent(EnumHandler.class);
			StrolchEnum sexEnum = enumHandler.getEnum(certificate, "sex", Locale.ENGLISH);
			assertEquals("sex", sexEnum.getName());
			assertEquals("en", sexEnum.getLocale());
			assertEquals(3, sexEnum.getValues().size());
			List<String> values = sexEnum.getEnumValues();
			Collections.sort(values);
			assertEquals("both", values.get(0));

			StrolchEnum salutationsEnum = enumHandler.getEnum(certificate, "salutations", Locale.UK);
			assertEquals("salutations", salutationsEnum.getName());
			assertEquals("en_GB", salutationsEnum.getLocale());
			assertEquals(3, salutationsEnum.getValues().size());
			values = salutationsEnum.getEnumValues();
			Collections.sort(values);
			assertEquals("Mr", values.get(0));

			StrolchEnum religionsEnum = enumHandler.getEnum(certificate, "religions", Locale.CANADA);
			assertEquals("religions", religionsEnum.getName());
			assertEquals("en_CA", religionsEnum.getLocale());
			assertEquals(9, religionsEnum.getValues().size());
			values = religionsEnum.getEnumValues();
			Collections.sort(values);
			assertEquals("Atheist", values.get(0));
		});
	}
}
