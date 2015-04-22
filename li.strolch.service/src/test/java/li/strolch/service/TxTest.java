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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import li.strolch.command.AddResourceCommand;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.AbstractRealmServiceTest;

import org.junit.Ignore;
import org.junit.Test;

import ch.eitchnet.utils.dbc.DBC;

public class TxTest extends AbstractRealmServiceTest {

	@Test
	public void shouldCommit() {

		runServiceInAllRealmTypes(CommitService.class, new ServiceArgument());
	}

	@Test
	public void shouldRollback() {

		runServiceInAllRealmTypes(RollbackService.class, new ServiceArgument());
	}

	@Test
	public void shouldDoNothing() {

		runServiceInAllRealmTypes(ReadonlyService.class, new ServiceArgument());
	}

	@Test
	@Ignore("We have to re-think this. It does not work, throwing an exception if commands are registered on a read-only TX as then we don't know if we want to roll back or not - we probably need a ROLLBACK_ON_ERROR or something")
	public void shouldNotAllowCommandsOnDoNothing() {

		runServiceInAllRealmTypes(ReadonlyFailService.class, new ServiceArgument());
	}

	public static class CommitService extends AbstractService<ServiceArgument, ServiceResult> {
		private static final long serialVersionUID = 1L;

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
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

				tx.commitOnClose();
			}

			// now make sure the new resource exists
			try (StrolchTransaction tx = openTx(arg.realm)) {

				Resource res = tx.getResourceBy(id, id);
				if (res == null) {
					throw tx.fail("Did not expect resource with id " + id);
				}
			}

			return ServiceResult.success();
		}
	}

	public static class RollbackService extends AbstractService<ServiceArgument, ServiceResult> {
		private static final long serialVersionUID = 1L;

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
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

				tx.rollbackOnClose();
			}

			// now make sure the new resource does not exist
			try (StrolchTransaction tx = openTx(arg.realm)) {

				Resource res = tx.getResourceBy(id, id);
				if (res != null) {
					throw tx.fail("Did not find expected resource with id " + id);
				}
			}

			return ServiceResult.success();
		}
	}

	public static class ReadonlyService extends AbstractService<ServiceArgument, ServiceResult> {
		private static final long serialVersionUID = 1L;

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
		}

		@Override
		protected ServiceResult internalDoService(ServiceArgument arg) throws Exception {

			try (StrolchTransaction tx = openTx(arg.realm)) {
				Resource yellowBall = tx.getResourceBy("Ball", "yellow");
				assertNotNull("Expected to find the yellow ball", yellowBall);

				Order myCarOrder = tx.getOrderBy("ProductionOrder", "myCarOrder");
				assertNotNull("Expected to find the my car order", myCarOrder);
			}

			return ServiceResult.success();
		}
	}

	public static class ReadonlyFailService extends AbstractService<ServiceArgument, ServiceResult> {
		private static final long serialVersionUID = 1L;

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
		}

		@Override
		protected ServiceResult internalDoService(ServiceArgument arg) throws Exception {

			String id = "flushSuccessfully";
			Resource resource = ModelGenerator.createResource(id, id, id);

			boolean txFailed = false;
			try (StrolchTransaction tx = openTx(arg.realm)) {
				AddResourceCommand addResCmd = new AddResourceCommand(getContainer(), tx);
				addResCmd.setResource(resource);
				tx.addCommand(addResCmd);
			} catch (Exception e) {
				// expected
				txFailed = true;
			}

			assertTrue("TX should have failed!", txFailed);

			return ServiceResult.success();
		}
	}
}
