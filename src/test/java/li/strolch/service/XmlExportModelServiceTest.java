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
package li.strolch.service;

import java.io.File;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.service.test.AbstractRealmServiceTest;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlExportModelServiceTest extends AbstractRealmServiceTest {

	private static final String TMP_XML_EXPORT_XML = "tmpXmlExport.xml";

	@Test
	public void runTest() {

		Runner before = new Runner() {
			@Override
			public void run(StrolchRealm strolchRealm, ComponentContainer container) {
				File file = new File(RUNTIME_PATH + "/data", TMP_XML_EXPORT_XML);
				if (file.exists())
					file.delete();
			}
		};

		XmlExportModelService svc = new XmlExportModelService();
		XmlExportModelArgument arg = new XmlExportModelArgument();
		arg.modelFileName = TMP_XML_EXPORT_XML;

		runServiceInAllRealmTypes(svc, arg, before, null, null);
	}
}
