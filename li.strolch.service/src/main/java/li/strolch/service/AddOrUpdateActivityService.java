/*
 * Copyright 2017 Reto Breitenmoser <reto.breitenmoser@gmail.com>
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

import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

/**
 * @author Reto Breitenmoser <reto.breitenmoser@gmail.com>
 */
public class AddOrUpdateActivityService
		extends AbstractService<AddOrUpdateActivityService.AddOrUpdateActivityArg, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public AddOrUpdateActivityArg getArgumentInstance() {
		return new AddOrUpdateActivityArg();
	}

	@Override
	protected ServiceResult internalDoService(AddOrUpdateActivityArg arg) {

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			if (tx.hasActivity(arg.activity.getType(), arg.activity.getId())) {
				tx.update(arg.activity);
			} else {
				tx.add(arg.activity);
			}

			tx.commitOnClose();
		}

		return ServiceResult.success();
	}

	public static class AddOrUpdateActivityArg extends ServiceArgument {
		private static final long serialVersionUID = 1L;
		public Activity activity;
	}
}
