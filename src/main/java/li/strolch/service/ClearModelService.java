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

import java.text.MessageFormat;

import li.strolch.command.ClearModelCommand;
import li.strolch.model.ModelStatistics;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ClearModelService extends AbstractService<ClearModelArgument, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	protected ServiceResult internalDoService(ClearModelArgument arg) {

		ClearModelCommand command;
		try (StrolchTransaction tx = openTx(arg.realm)) {

			command = new ClearModelCommand(getContainer(), tx);
			command.setClearOrders(arg.clearOrders);
			command.setClearResources(arg.clearResources);
			tx.addCommand(command);
		}

		ModelStatistics statistics = command.getStatistics();
		String durationS = StringHelper.formatNanoDuration(statistics.durationNanos);
		logger.info(MessageFormat.format(
				"Clearing Model for realm {1} took {2}.", arg.realm, durationS)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Cleared {0} Orders", statistics.nrOfOrders)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Cleared {0} Resources", statistics.nrOfResources)); //$NON-NLS-1$

		return ServiceResult.success();
	}
}
