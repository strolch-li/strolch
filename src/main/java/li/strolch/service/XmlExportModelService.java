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
import java.text.MessageFormat;

import li.strolch.command.XmlExportModelCommand;
import li.strolch.exception.StrolchException;
import li.strolch.model.xml.XmlModelSaxReader.XmlModelStatistics;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlExportModelService extends AbstractService<XmlExportModelArgument, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	protected ServiceResult internalDoService(XmlExportModelArgument arg) {

		DBC.PRE.assertNotEmpty("Modelfile must be set!", arg.modelFileName);

		File dataPath = getRuntimeConfiguration().getDataPath();
		File modelFile = new File(dataPath, arg.modelFileName);
		if (modelFile.exists()) {
			String msg = "Model File already exists with name {0} in data path {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, arg.modelFileName, dataPath);
			throw new StrolchException(msg);
		}

		logger.info("Exporting: " + arg);
		logger.info("Exporting model to real path: " + modelFile.getAbsolutePath());

		XmlExportModelCommand command;
		try (StrolchTransaction tx = openTx(arg.realm)) {

			command = new XmlExportModelCommand(getContainer(), tx);
			command.setModelFile(modelFile);
			command.setMultiFile(arg.multiFile);
			command.setDoOrders(arg.doOrders);
			command.setDoResources(arg.doResources);
			command.setOrderTypes(arg.orderTypes);
			command.setResourceTypes(arg.resourceTypes);
			tx.addCommand(command);
		}

		XmlModelStatistics statistics = command.getStatistics();
		String msg = "Wrote XML Model file {0} for realm {1}: {2} at path: {3}";
		logger.info(MessageFormat.format(msg, modelFile.getName(), arg.realm, statistics, modelFile.getAbsolutePath()));
		return ServiceResult.success();
	}
}
