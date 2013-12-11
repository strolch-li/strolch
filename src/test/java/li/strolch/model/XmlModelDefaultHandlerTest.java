/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.model;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.xml.StrolchElementListener;
import li.strolch.model.xml.XmlModelDefaultHandler;
import li.strolch.model.xml.XmlModelDefaultHandler.XmlModelStatistics;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class XmlModelDefaultHandlerTest {

	private static final Logger logger = LoggerFactory.getLogger(XmlModelDefaultHandlerTest.class);

	@Test
	public void shouldParseXmlModelFile() {

		final Map<String, Resource> resourceMap = new HashMap<>();
		final Map<String, Order> orderMap = new HashMap<>();

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
		};
		XmlModelDefaultHandler handler = new XmlModelDefaultHandler(listener, file);
		handler.parseFile();

		assertEquals(3, resourceMap.size());
		assertEquals(3, orderMap.size());

		XmlModelStatistics statistics = handler.getStatistics();
		logger.info("Parsing took " + StringHelper.formatNanoDuration(statistics.durationNanos));
		assertEquals(3, statistics.nrOfOrders);
		assertEquals(3, statistics.nrOfResources);
	}
}
