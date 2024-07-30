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
import li.strolch.service.api.ServiceResultState;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ClearModelService extends AbstractService<ClearModelArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public ClearModelArgument getArgumentInstance() {
		return new ClearModelArgument();
	}

	@Override
	protected ServiceResult internalDoService(ClearModelArgument arg) {

		ClearModelCommand command;
		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			command = new ClearModelCommand(tx);
			command.setClearOrders(arg.clearOrders);
			command.setClearResources(arg.clearResources);
			tx.addCommand(command);
			tx.commitOnClose();
		}

		ModelStatistics statistics = command.getStatistics();
		String durationS = StringHelper.formatNanoDuration(statistics.durationNanos);
		logger.info(MessageFormat.format("Clearing Model for realm {0} took {1}.", arg.realm, durationS));
		logger.info(MessageFormat.format("Cleared {0} Orders", statistics.nrOfOrders));
		logger.info(MessageFormat.format("Cleared {0} Resources", statistics.nrOfResources));

		return ServiceResult.success();
	}
}
