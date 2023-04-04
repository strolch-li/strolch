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
package li.strolch.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

import java.util.List;

import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.search.ResourceSearch;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.AbstractRealmServiceTest;
import org.junit.Test;

public class TxModifyTest extends AbstractRealmServiceTest<ServiceArgument, ServiceResult> {

	private Class<? extends Service<ServiceArgument, ServiceResult>> svcClass;

	@Test
	public void shouldFindModifiedElementInSearch() {
		this.svcClass = ModifyAndSearchService.class;
		runServiceInAllRealmTypes();
	}

	@Test
	public void shouldRollbackSuccessfully() {
		this.svcClass = RollbackService.class;
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

	public static class ModifyAndSearchService extends AbstractService<ServiceArgument, ServiceResult> {

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

			String type = "Car";
			String id = "car001";
			Resource resource = ModelGenerator.createResource(id, id, type);
			try (StrolchTransaction tx = openTx(arg.realm)) {
				tx.add(resource);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = openTx(arg.realm)) {

				// get the element
				Resource car001 = tx.getResourceBy(type, id, true);

				// modify
				car001.setName("Changed!");
				tx.update(car001);

				// search for all cars
				List<Resource> cars = new ResourceSearch().types(type).search(tx).cloneIfReadOnly().toList();
				assertEquals(1, cars.size());
				assertSame("We didn't get the same car!", car001, cars.get(0));

				// also validate get returns the same instance
				Resource carByGet = tx.getResourceBy(type, id, true);
				assertSame("We didn't get the same car!", car001, carByGet);

				// also validate find returns the same instance
				Resource carByFind = tx.findElement(car001.getLocator());
				assertSame("We didn't get the same car!", car001, carByFind);

				tx.commitOnClose();
			}

			return ServiceResult.success();
		}
	}

	public static class RollbackService extends AbstractService<ServiceArgument, ServiceResult> {

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

			String type = "Car";
			String id = "car002";
			Resource resource = ModelGenerator.createResource(id, id, type);
			try (StrolchTransaction tx = openTx(arg.realm)) {
				tx.add(resource);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = openTx(arg.realm)) {
				Resource car001 = tx.getResourceBy(type, id, true);

				// modify
				car001.setName("Changed!");
				tx.update(car001);

				tx.rollbackOnClose();
			}

			// now make sure the element was not changed
			try (StrolchTransaction tx = openTx(arg.realm)) {
				Resource car001 = tx.getResourceBy(type, id, true);
				assertEquals("Expected name not to be changed in rollback of TX!", id, car001.getName());
			}

			return ServiceResult.success();
		}
	}
}
