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

package li.strolch.model;

import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.xml.StrolchXmlHelper;
import org.junit.Test;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertEquals;

public class StrolchXmlHelperTest {

	@Test
	public void shouldWriteAndReadXml() {

		File outFile = new File("target/" + getClass().getSimpleName() + ".xml");

		Activity activity = ModelGenerator.createActivity("activity", "Activity", "Activity", TimeOrdering.SERIES);
		Resource resource = ModelGenerator.createResource("res", "Res", "Res");
		Order order = ModelGenerator.createOrder("order", "Order", "Order");

		Set<StrolchRootElement> elements = new HashSet<>(Arrays.asList(activity, resource, order));

		StrolchXmlHelper.writeToFile(outFile, elements);
		Set<StrolchRootElement> result = new HashSet<>(StrolchXmlHelper.parseFile(outFile));

		assertEquals(elements, result);
	}
}
