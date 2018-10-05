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

import static li.strolch.testbase.runtime.RuntimeMock.assertServiceResult;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertThat;

import java.io.File;
import java.io.IOException;

import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.service.test.AbstractRealmServiceTest;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlExportModelServiceTest extends AbstractRealmServiceTest<XmlExportModelArgument, ServiceResult> {

	private static final String TMP_XML_EXPORT_XML = "tmpXmlExport.xml";

	@Override
	protected Class<? extends Service<XmlExportModelArgument, ServiceResult>> getSvcClass() {
		return XmlExportModelService.class;
	}

	@Override
	protected XmlExportModelArgument getArgInstance() {

		XmlExportModelArgument arg = new XmlExportModelArgument();
		arg.modelFileName = TMP_XML_EXPORT_XML;
		arg.multiFile = true;

		return arg;
	}

	@Test
	public void runTest() {

		Runner before = (strolchRealm, container) -> {
			File file = new File(RUNTIME_PATH + "/data", TMP_XML_EXPORT_XML);
			if (file.exists())
				if (!file.delete())
					throw new IllegalStateException("Failed to delete " + file.getAbsolutePath());
		};

		runServiceInAllRealmTypes(before, null, null);
	}

	@Test
	public void runTestFailOnExisting() {
		File file = new File(RUNTIME_PATH + "/data", TMP_XML_EXPORT_XML);
		if (!file.exists()) {
			try {
				if (!file.createNewFile())
					throw new IllegalStateException("Could not create new file " + file.getAbsolutePath());
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

		XmlExportModelService svc = new XmlExportModelService();
		XmlExportModelArgument arg = new XmlExportModelArgument();
		arg.realm = REALM_TRANSIENT;
		arg.overwrite = false;
		arg.multiFile = true;
		arg.modelFileName = TMP_XML_EXPORT_XML;

		ServiceResult result = getServiceHandler().doService(this.certificate, svc, arg);
		assertServiceResult(ServiceResultState.FAILED, ServiceResult.class, result);
		assertThat(result.getMessage(), containsString("Model File already exists with name"));
	}

	@Test
	public void runTestOverwrite() {
		File file = new File(RUNTIME_PATH + "/data", TMP_XML_EXPORT_XML);
		if (!file.exists()) {
			try {
				if (!file.createNewFile())
					throw new IllegalStateException("Could not create new file " + file.getAbsolutePath());
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

		XmlExportModelService svc = new XmlExportModelService();
		XmlExportModelArgument arg = new XmlExportModelArgument();
		arg.realm = REALM_TRANSIENT;
		arg.overwrite = true;
		arg.multiFile = true;
		arg.modelFileName = TMP_XML_EXPORT_XML;

		ServiceResult result = getServiceHandler().doService(this.certificate, svc, arg);
		assertServiceResult(ServiceResultState.SUCCESS, ServiceResult.class, result);
	}

}
