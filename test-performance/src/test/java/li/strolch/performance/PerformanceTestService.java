/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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

import static li.strolch.model.ModelGenerator.BAG_ID;
import static li.strolch.model.ModelGenerator.PARAM_STRING_ID;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResultState;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.helper.SystemHelper;

public class PerformanceTestService extends AbstractService<PerformanceTestArgument, PerformanceTestResult> {

	private static final String MY_TYPE = "MyType";

	private PerformanceTestArgument arg;

	@Override
	protected PerformanceTestResult getResultInstance() {
		return new PerformanceTestResult(ServiceResultState.FAILED);
	}

	@Override
	public PerformanceTestArgument getArgumentInstance() {
		return new PerformanceTestArgument();
	}

	@Override
	protected PerformanceTestResult internalDoService(PerformanceTestArgument arg) {

		this.arg = arg;
		long start = System.currentTimeMillis();

		long resolutionMillis = TimeUnit.SECONDS.toMillis(1);
		long resolutionSeconds = TimeUnit.MILLISECONDS.toSeconds(resolutionMillis);
		long nextLog = start + resolutionMillis;
		long allTx = 0;
		long nrOfTx = 0;

		List<String> resourceIds = new ArrayList<>();

		while (run(start)) {

			try (StrolchTransaction tx = openUserTx()) {

				if (resourceIds.isEmpty()) {
					for (int i = 0; i < arg.nrOfElements; i++) {
						resourceIds.add(createResource(tx));
					}
				} else {

					if (resourceIds.size() > 10) {

						int removeTo = resourceIds.size() / 3;
						int updateTo = (resourceIds.size() / 3) * 2;

						// we have many, so update some, change some, create some
						List<String> toRemove = new ArrayList<>(resourceIds.subList(0, removeTo));
						List<String> toUpdate = new ArrayList<>(resourceIds.subList(removeTo, updateTo));
						resourceIds.removeAll(toRemove);

						for (String resourceId : toRemove) {
							tx.remove(tx.getResourceBy(MY_TYPE, resourceId, true));
						}
						for (String resourceId : toUpdate) {
							Resource resource = tx.getResourceBy(MY_TYPE, resourceId, true);
							StringParameter stringP = resource.getParameter(BAG_ID, PARAM_STRING_ID);
							stringP.setValue("Yellow!");
							tx.update(resource);
						}
						for (int i = 0; i < removeTo; i++) {
							resourceIds.add(createResource(tx));
						}

					} else {

						for (Iterator<String> iterator = resourceIds.iterator(); iterator.hasNext(); ) {
							String resourceId = iterator.next();
							tx.remove(tx.getResourceBy(MY_TYPE, resourceId, true));
							iterator.remove();
						}
					}
				}

				tx.commitOnClose();
			}

			nrOfTx++;
			allTx++;
			long now = System.currentTimeMillis();
			if (System.currentTimeMillis() >= nextLog) {
				nextLog = now + resolutionMillis;
				logger.info(
						getCertificate().getSessionId() + ": Performing " + (nrOfTx / resolutionSeconds) + " TXs/s ("
								+ SystemHelper.getMemorySummary() + ")");
				nrOfTx = 0;
			}
		}

		long end = System.currentTimeMillis();
		long took = end - start;
		long txPerSec = allTx / (took / 1000);
		logger.info("Took " + StringHelper.formatMillisecondsDuration(took) + " for " + allTx + " TXs with " + txPerSec
				+ " TXs/s");

		return new PerformanceTestResult(allTx);
	}

	private boolean run(long start) {
		return System.currentTimeMillis() < start + this.arg.unit.toMillis(this.arg.duration);
	}

	private String createResource(StrolchTransaction tx) {
		String id = StrolchAgent.getUniqueId();
		Resource resource = ModelGenerator.createResource(id, id, MY_TYPE);
		tx.add(resource);
		return resource.getId();
	}
}