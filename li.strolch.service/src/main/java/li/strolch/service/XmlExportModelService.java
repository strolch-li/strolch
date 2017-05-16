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
import li.strolch.model.ModelStatistics;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlExportModelService extends AbstractService<XmlExportModelArgument, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public XmlExportModelArgument getArgumentInstance() {
		return new XmlExportModelArgument();
	}

	@Override
	protected ServiceResult internalDoService(XmlExportModelArgument arg) {

		DBC.PRE.assertNotEmpty("Modelfile must be set!", arg.modelFileName);

		File modelFile;

		File dataPath = getRuntimeConfiguration().getDataPath();
		File modelFileName = new File(arg.modelFileName);
		if (modelFileName.isAbsolute()) {
			if (!arg.external)
				return ServiceResult.error("Model file is absolute, yet service argument is not marked as external!");
			modelFile = modelFileName;
		} else {
			if (arg.external)
				return ServiceResult.error("Model file is not absolute, yet service argument is marked as externa!");

			modelFile = new File(dataPath, arg.modelFileName);
		}

		if (modelFile.exists()) {
			if (!arg.overwrite) {
				if (modelFileName.isAbsolute()) {
					String msg = "Model File already exists at path {0}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, arg.modelFileName);
					return ServiceResult.error(msg);
				}

				String msg = "Model File already exists with name {0} in data path {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, arg.modelFileName, dataPath);
				return ServiceResult.error(msg);
			}
		}

		logger.info("Exporting: " + arg);
		logger.info("Exporting model to real path: " + modelFile.getAbsolutePath());

		XmlExportModelCommand command;
		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			command = new XmlExportModelCommand(getContainer(), tx);
			command.setModelFile(modelFile);
			command.setOverwrite(arg.overwrite);
			command.setMultiFile(arg.multiFile);
			command.setDoOrders(arg.doOrders);
			command.setDoResources(arg.doResources);
			command.setDoActivities(arg.doActivities);
			command.setOrderTypes(arg.orderTypes);
			command.setResourceTypes(arg.resourceTypes);
			command.setActivityTypes(arg.activityTypes);

			tx.addCommand(command);
			tx.commitOnClose();
		}

		ModelStatistics statistics = command.getStatistics();
		String msg = "Wrote XML Model file {0} for realm {1}: {2} at path: {3}";
		logger.info(MessageFormat.format(msg, modelFile.getName(), arg.realm, statistics, modelFile.getAbsolutePath()));
		logger.info(MessageFormat.format("Wrote {0} Orders", statistics.nrOfOrders)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Wrote {0} Resources", statistics.nrOfResources)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Wrote {0} Activities", statistics.nrOfActivities)); //$NON-NLS-1$
		return ServiceResult.success();
	}
}
