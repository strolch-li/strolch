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
package li.strolch.operationslog;

import li.strolch.service.api.AbstractService;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.AbstractRealmServiceTest;
import li.strolch.testbase.runtime.LogMessagesTestRunner;
import org.junit.Test;

public class OperationsLogTest extends AbstractRealmServiceTest<ServiceArgument, ServiceResult> {

	private Class<? extends Service<ServiceArgument, ServiceResult>> svcClass;

	@Test
	public void shouldDoOperationsLog() {
		this.svcClass = OperationsLogService.class;
		runServiceInAllRealmTypes();
	}

	@Override
	protected Class<? extends Service<ServiceArgument, ServiceResult>> getSvcClass() {
		return this.svcClass;
	}

	@Override
	protected ServiceArgument getArgInstance() {
		return new ServiceArgument();
	}

	public static class OperationsLogService extends AbstractService<ServiceArgument, ServiceResult> {
		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
		}

		@Override
		public ServiceArgument getArgumentInstance() {
			return new ServiceArgument();
		}

		@Override
		protected ServiceResult internalDoService(ServiceArgument arg) {

			new LogMessagesTestRunner(getContainer(), arg.realm).runLogMessagesTest();

			return ServiceResult.success();
		}
	}
}
