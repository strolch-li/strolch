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

import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.AddResourceCommand;
import li.strolch.persistence.api.RemoveResourceCommand;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.UpdateResourceCommand;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.AbstractRealmServiceTest;
import li.strolch.utils.dbc.DBC;
import org.junit.Test;

public class FlushTxTest extends AbstractRealmServiceTest<ServiceArgument, ServiceResult> {

	private Class<? extends Service<ServiceArgument, ServiceResult>> svcClass;

	@Test
	public void shouldFlushSuccessfully1() {
		this.svcClass = FlushingCommandsService1.class;
		runServiceInAllRealmTypes();
	}

	@Test
	public void shouldFlushSuccessfully2() {
		this.svcClass = FlushingCommandsService2.class;
		runServiceInAllRealmTypes();
	}

	@Test
	public void shouldRollbackSuccessfully() {
		this.svcClass = RollbackAfterFlushCommandsService.class;
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

	public static class FlushingCommandsService1 extends AbstractService<ServiceArgument, ServiceResult> {

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
		}

		@Override
		public ServiceArgument getArgumentInstance() {
			return new ServiceArgument();
		}

		@Override
		protected ServiceResult internalDoService(ServiceArgument arg) throws Exception {

			String id = "flushSuccessfully";
			Resource resource = ModelGenerator.createResource(id, id, id);

			try (StrolchTransaction tx = openTx(arg.realm)) {

				DBC.PRE.assertNull("Did not expect resource with id " + id, tx.getResourceBy(id, id));

				AddResourceCommand addResCmd = new AddResourceCommand(getContainer(), tx);
				addResCmd.setResource(resource);
				tx.addCommand(addResCmd);
				tx.flush();
				DBC.PRE.assertNotNull("Expected resource with id " + id, tx.getResourceBy(id, id));

				RemoveResourceCommand rmResCmd = new RemoveResourceCommand(getContainer(), tx);
				rmResCmd.setResource(resource);
				tx.addCommand(rmResCmd);
				tx.flush();
				DBC.PRE.assertNull("Expect to remove resource with id " + id, tx.getResourceBy(id, id));

				tx.commitOnClose();
			}

			// now make sure the new resource does not exist
			try (StrolchTransaction tx = openTx(arg.realm)) {

				Resource res = tx.getResourceBy(id, id);
				if (res != null) {
					throw tx.fail("Did not expect resource with id " + id);
				}
			}

			return ServiceResult.success();
		}
	}

	public static class FlushingCommandsService2 extends AbstractService<ServiceArgument, ServiceResult> {

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
		}

		@Override
		public ServiceArgument getArgumentInstance() {
			return new ServiceArgument();
		}

		@Override
		protected ServiceResult internalDoService(ServiceArgument arg) throws Exception {

			String id = "flushSuccessfully";
			Resource resource = ModelGenerator.createResource(id, id, id);

			try (StrolchTransaction tx = openTx(arg.realm)) {

				DBC.PRE.assertNull("Did not expect resource with id " + id, tx.getResourceBy(id, id));

				AddResourceCommand addResCmd = new AddResourceCommand(getContainer(), tx);
				addResCmd.setResource(resource);
				tx.addCommand(addResCmd);
				tx.flush();
				DBC.PRE.assertNotNull("Expected resource with id " + id, tx.getResourceBy(id, id));

				Resource res = tx.getResourceBy(id, id);

				UpdateResourceCommand updateResCmd = new UpdateResourceCommand(getContainer(), tx);
				updateResCmd.setResource(res);
				tx.addCommand(updateResCmd);

				tx.commitOnClose();
			}

			// now make sure the new resource does exist
			try (StrolchTransaction tx = openTx(arg.realm)) {

				Resource res = tx.getResourceBy(id, id);
				if (res == null) {
					throw tx.fail("Did not find expected resource with id " + id);
				}
			}

			return ServiceResult.success();
		}
	}

	public static class RollbackAfterFlushCommandsService extends AbstractService<ServiceArgument, ServiceResult> {

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
		}

		@Override
		public ServiceArgument getArgumentInstance() {
			return new ServiceArgument();
		}

		@Override
		protected ServiceResult internalDoService(ServiceArgument arg) throws Exception {

			String id = "flushSuccessfully2";
			Resource resource = ModelGenerator.createResource(id, id, id);

			try (StrolchTransaction tx = openTx(arg.realm)) {

				DBC.PRE.assertNull("Did not expect resource with id " + id, tx.getResourceBy(id, id));

				AddResourceCommand addResCmd = new AddResourceCommand(getContainer(), tx);
				addResCmd.setResource(resource);
				tx.addCommand(addResCmd);
				tx.flush();
				DBC.PRE.assertNotNull("Expected resource with id " + id, tx.getResourceBy(id, id));

				// now force a rollback
				tx.rollbackOnClose();
			}

			// now make sure the new resource does not exist
			try (StrolchTransaction tx = openTx(arg.realm)) {

				Resource res = tx.getResourceBy(id, id);
				if (res != null) {
					throw tx.fail("Did not expect resource with id after rolling back previous TX " + id);
				}
			}

			// now do it over, but use throw
			try (StrolchTransaction tx = openTx(arg.realm)) {

				DBC.PRE.assertNull("Did not expect resource with id " + id, tx.getResourceBy(id, id));

				AddResourceCommand addResCmd = new AddResourceCommand(getContainer(), tx);
				addResCmd.setResource(resource);
				tx.addCommand(addResCmd);
				tx.flush();
				DBC.PRE.assertNotNull("Expected resource with id " + id, tx.getResourceBy(id, id));

				// now force a rollback
				throw tx.fail("Oh snap, something went wrong!");

			} catch (Exception e) {
				// expected
			}

			// now make sure the new resource does not exist
			try (StrolchTransaction tx = openTx(arg.realm)) {

				Resource res = tx.getResourceBy(id, id);
				if (res != null) {
					throw tx.fail("Did not expect resource with id after rolling back previous TX " + id);
				}
			}

			return ServiceResult.success();
		}
	}
}
