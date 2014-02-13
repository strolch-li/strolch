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

import java.util.Locale;

import li.strolch.agent.ComponentContainerTest;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.runtime.query.enums.EnumHandler;
import li.strolch.runtime.query.enums.StrolchEnum;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class EnumHandlerTest {

	private static final String ENUM_HANDLER_TEST_RUNTIME = "target/EnumHandlerTest/";

	@Test
	public void shouldFindByLocator() {

		StrolchAgent agent = ComponentContainerTest.startContainer(ENUM_HANDLER_TEST_RUNTIME,
				ComponentContainerTest.PATH_TRANSIENT_CONTAINER);
		ComponentContainer container = agent.getContainer();

		EnumHandler enumHandler = container.getComponent(EnumHandler.class);
		StrolchEnum sexEnum = enumHandler.getEnum("sex", Locale.ENGLISH);
		assertEquals("sex", sexEnum.getName());
		assertEquals("en", sexEnum.getLocale());
		assertEquals(3, sexEnum.getValues().size());
		assertEquals("both", sexEnum.getValues().get(0).getValue());

		StrolchEnum salutationsEnum = enumHandler.getEnum("salutations", Locale.UK);
		assertEquals("salutations", salutationsEnum.getName());
		assertEquals("en_GB", salutationsEnum.getLocale());
		assertEquals(3, salutationsEnum.getValues().size());
		assertEquals("Mr", salutationsEnum.getValues().get(0).getValue());

		StrolchEnum religionsEnum = enumHandler.getEnum("religions", Locale.CANADA);
		assertEquals("religions", religionsEnum.getName());
		assertEquals("en_CA", religionsEnum.getLocale());
		assertEquals(9, religionsEnum.getValues().size());
		assertEquals("Orthodox", religionsEnum.getValues().get(0).getValue());
	}
}
