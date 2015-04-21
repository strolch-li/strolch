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
package li.strolch.performance;

import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.command.AddResourceCommand;
import li.strolch.command.RemoveResourceCommand;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;

public class PerformanceTestService extends AbstractService<ServiceArgument, ServiceResult> {

	private static final String MY_TYPE = "MyType";
	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	protected boolean isArgumentRequired() {
		return false;
	}

	@Override
	protected ServiceResult internalDoService(ServiceArgument arg) throws Exception {

		long start = System.currentTimeMillis();

		long resolutionMillis = TimeUnit.SECONDS.toMillis(1);
		long resolutionSeconds = TimeUnit.MILLISECONDS.toSeconds(resolutionMillis);
		long nextLog = start + resolutionMillis;
		long nrOfTx = 0;

		String resId = null;
		
		while (run(start)) {

			try (StrolchTransaction tx = openUserTx()) {

				if (resId != null) {
					Resource toDelete = queryResource(tx, resId);
					deleteResource(tx, toDelete);
				}
				resId = 
				createResource(tx);

				tx.commitOnClose();
			}

			nrOfTx++;
			long now = System.currentTimeMillis();
			if (System.currentTimeMillis() >= nextLog) {
				nextLog = now + resolutionMillis;
				logger.info("Performing " + (nrOfTx / resolutionSeconds) + " TXs/s");
				nrOfTx = 0;
			}
		}

		return ServiceResult.success();
	}

	private boolean run(long start) {
		return System.currentTimeMillis() < start + TimeUnit.MINUTES.toMillis(5);
	}

	private void deleteResource(StrolchTransaction tx, Resource toDelete) {
		RemoveResourceCommand cmd = new RemoveResourceCommand(getContainer(), tx);
		cmd.setResource(toDelete);
		tx.addCommand(cmd);
	}

	private Resource queryResource(StrolchTransaction tx, String resId) {
		return tx.getResourceBy(MY_TYPE, resId);
	}

	private String createResource(StrolchTransaction tx) {
		String id = StrolchAgent.getUniqueId();
		Resource resource = ModelGenerator.createResource(id, id, MY_TYPE);

		AddResourceCommand cmd = new AddResourceCommand(getContainer(), tx);
		cmd.setResource(resource);
		tx.addCommand(cmd);

		return id;
	}
}