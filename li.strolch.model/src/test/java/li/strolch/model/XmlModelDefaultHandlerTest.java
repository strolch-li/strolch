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
package li.strolch.model;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import li.strolch.model.activity.Activity;
import li.strolch.model.xml.StrolchElementListener;
import li.strolch.model.xml.XmlModelSaxFileReader;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class XmlModelDefaultHandlerTest {

	private static final Logger logger = LoggerFactory.getLogger(XmlModelDefaultHandlerTest.class);

	@Test
	public void shouldParseXmlModelFile() {

		Map<String, Resource> resourceMap = new HashMap<>();
		Map<String, Order> orderMap = new HashMap<>();
		Map<String, Activity> activityMap = new HashMap<>();

		File file = new File("src/test/resources/data/StrolchModel.xml");
		StrolchElementListener listener = new StrolchElementListener() {
			@Override
			public void notifyResource(Resource resource) {
				resourceMap.put(resource.getId(), resource);
			}

			@Override
			public void notifyOrder(Order order) {
				orderMap.put(order.getId(), order);
			}

			@Override
			public void notifyActivity(Activity activity) {
				activityMap.put(activity.getId(), activity);
			}
		};

		XmlModelSaxFileReader handler = new XmlModelSaxFileReader(listener, file, true);
		handler.parseFile();

		assertEquals(3, resourceMap.size());
		assertEquals(3, orderMap.size());
		assertEquals(3, activityMap.size());

		ModelStatistics statistics = handler.getStatistics();
		logger.info("Parsing took " + StringHelper.formatNanoDuration(statistics.durationNanos));
		assertEquals(3, statistics.nrOfOrders);
		assertEquals(3, statistics.nrOfResources);
		assertEquals(3, statistics.nrOfActivities);
	}
}
