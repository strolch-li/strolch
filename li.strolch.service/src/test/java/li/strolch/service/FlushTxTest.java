package li.strolch.service;

import li.strolch.command.AddResourceCommand;
import li.strolch.command.RemoveResourceCommand;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.TransactionCloseStrategy;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.AbstractRealmServiceTest;

import org.junit.Test;

import ch.eitchnet.utils.dbc.DBC;

public class FlushTxTest extends AbstractRealmServiceTest {

	@Test
	public void shouldFlushSuccessfully() {

		runServiceInAllRealmTypes(FlushingCommandsService.class, new ServiceArgument());
	}

	@Test
	public void shouldRollbackSuccessfully() {

		runServiceInAllRealmTypes(RollbackAfterFlushCommandsService.class, new ServiceArgument());
	}

	public static class FlushingCommandsService extends AbstractService<ServiceArgument, ServiceResult> {
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

				RemoveResourceCommand rmResCmd = new RemoveResourceCommand(getContainer(), tx);
				rmResCmd.setResource(resource);
				tx.addCommand(rmResCmd);
				tx.flush();
				DBC.PRE.assertNull("Expect to remove resource with id " + id, tx.getResourceBy(id, id));
			}

			return ServiceResult.success();
		}
	}

	public static class RollbackAfterFlushCommandsService extends AbstractService<ServiceArgument, ServiceResult> {
		private static final long serialVersionUID = 1L;

		@Override
		protected ServiceResult getResultInstance() {
			return new ServiceResult();
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
				tx.setCloseStrategy(TransactionCloseStrategy.ROLLBACK);
			}

			// now make sure the new resource does not exist
			try (StrolchTransaction tx = openTx(arg.realm)) {
				DBC.PRE.assertNull("Did not expect resource with id after rolling back previous TX " + id,
						tx.getResourceBy(id, id));
			}

			return ServiceResult.success();
		}
	}
}
