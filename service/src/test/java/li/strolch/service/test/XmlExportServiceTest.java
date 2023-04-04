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
package li.strolch.service.test;

import static li.strolch.model.StrolchModelConstants.TEMPLATE;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FilenameFilter;

import li.strolch.privilege.model.Certificate;
import li.strolch.service.*;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlExportServiceTest {

	private static final String RUNTIME_PATH = "target/transienttest/"; //$NON-NLS-1$
	private static final String CONFIG_SRC = "src/test/resources/transienttest"; //$NON-NLS-1$
	protected static RuntimeMock runtimeMock;
	private static Certificate certificate;

	@BeforeClass
	public static void beforeClass() {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());
	}

	@AfterClass
	public static void afterClass() {
		runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldExportToXmlSingleFile() {

		XmlExportModelService service = new XmlExportModelService();
		XmlExportModelArgument arg = new XmlExportModelArgument();
		arg.modelFileName = "TestExportSingle.xml";
		arg.multiFile = false;
		ServiceResult result = runtimeMock.getServiceHandler().doService(certificate, service, arg);
		RuntimeMock.assertServiceResult(ServiceResultState.SUCCESS, ServiceResult.class, result);
		assertNumberOfFilesCreated(arg.modelFileName.split("\\.")[0], 1);

		importModel(arg.modelFileName);
	}

	@Test
	public void shouldExportToXmlMultiFile() {

		XmlExportModelService service = new XmlExportModelService();
		XmlExportModelArgument arg = new XmlExportModelArgument();
		arg.modelFileName = "TestExportMulti.xml";
		arg.multiFile = true;
		ServiceResult result = runtimeMock.getServiceHandler().doService(certificate, service, arg);
		RuntimeMock.assertServiceResult(ServiceResultState.SUCCESS, ServiceResult.class, result);
		assertNumberOfFilesCreated(arg.modelFileName.split("\\.")[0], 7);

		importModel(arg.modelFileName);
	}

	@Test
	public void shouldExportToXmlMultiFileOnlyResourceTemplates() {

		XmlExportModelService service = new XmlExportModelService();
		XmlExportModelArgument arg = new XmlExportModelArgument();
		arg.modelFileName = "TestExportOnlyResTemplates.xml";
		arg.doOrders = false;
		arg.resourceTypes.add(TEMPLATE);
		arg.multiFile = true;
		ServiceResult result = runtimeMock.getServiceHandler().doService(certificate, service, arg);
		RuntimeMock.assertServiceResult(ServiceResultState.SUCCESS, ServiceResult.class, result);
		assertNumberOfFilesCreated(arg.modelFileName.split("\\.")[0], 2);

		importModel(arg.modelFileName);
	}

	private void importModel(String modelFileName) {
		XmlImportModelService importService = new XmlImportModelService();
		XmlImportModelArgument importArgument = new XmlImportModelArgument();
		importArgument.modelFileName = modelFileName;
		ServiceResult result = runtimeMock.getServiceHandler().doService(certificate, importService, importArgument);
		RuntimeMock.assertServiceResult(ServiceResultState.SUCCESS, XmlImportModelResult.class, result);
	}

	private void assertNumberOfFilesCreated(final String modelFileName, int nrOfExpectedFiles) {
		File dataPath = new File(RUNTIME_PATH, "data");
		String[] list = dataPath.list((dir, name) -> name.startsWith(modelFileName));
		assertEquals(nrOfExpectedFiles, list.length);
	}
}
