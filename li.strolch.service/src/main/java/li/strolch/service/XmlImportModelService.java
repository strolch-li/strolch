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

import li.strolch.command.XmlImportModelCommand;
import li.strolch.exception.StrolchException;
import li.strolch.model.ModelStatistics;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlImportModelService extends AbstractService<XmlImportModelArgument, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	protected ServiceResult internalDoService(XmlImportModelArgument arg) {

		File dataPath = getRuntimeConfiguration().getDataPath();
		File modelFile = new File(dataPath, arg.modelFileName);
		if (!modelFile.exists()) {
			String msg = "Model File does not exist with name {0} in data path {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, arg.modelFileName, dataPath);
			throw new StrolchException(msg);
		}

		XmlImportModelCommand command;
		try (StrolchTransaction tx = openTx(arg.realm)) {

			command = new XmlImportModelCommand(getContainer(), tx);
			command.setModelFile(modelFile);
			command.setAddOrders(arg.addOrders);
			command.setAddResources(arg.addResources);
			command.setUpdateOrders(arg.updateOrders);
			command.setUpdateResources(arg.updateResources);
			command.setOrderTypes(arg.orderTypes);
			command.setResourceTypes(arg.resourceTypes);

			tx.addCommand(command);
			tx.commitOnClose();
		}

		ModelStatistics statistics = command.getStatistics();
		String durationS = StringHelper.formatNanoDuration(statistics.durationNanos);
		logger.info(MessageFormat.format(
				"Loading XML Model file {0} for realm {1} took {2}.", modelFile.getName(), arg.realm, durationS)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Loaded {0} Orders", statistics.nrOfOrders)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Loaded {0} Resources", statistics.nrOfResources)); //$NON-NLS-1$

		return ServiceResult.success();
	}
}
