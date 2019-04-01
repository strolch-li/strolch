package li.strolch.execution;

import static li.strolch.execution.policy.ReservationExection.PARAM_RESERVED;
import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static org.junit.Assert.*;

import java.io.File;

import li.strolch.execution.service.StartActivityExecutionService;
import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.service.LocatorArgument;
import li.strolch.service.parameter.SetParameterService;
import li.strolch.service.parameter.SetParameterService.SetParameterArg;
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ReservationExecutionTest extends RuntimeMock {

	@Before
	public void before() {
		mockRuntime(new File("target/" + ReservationExecutionTest.class.getName()),
				new File("src/test/resources/executiontest"));
		startContainer();
	}

	@After
	public void after() {
		destroyRuntime();
	}

	@Test
	public void shouldNotExecuteWhenReserved() throws InterruptedException {

		Locator activityLoc = Locator.valueOf(Tags.ACTIVITY, "ToStock", "produceMachine1");

		Certificate cert = loginTest();

		// before we execute, we reserve, so execution can't be done
		SetParameterService setParamSvc = new SetParameterService();
		SetParameterArg setParamArg = new SetParameterArg();
		setParamArg.locator = Locator
				.valueOf(Tags.RESOURCE, "Machine", "machine1", Tags.BAG, BAG_PARAMETERS, PARAM_RESERVED);
		setParamArg.valueAsString = "true";

		doServiceAssertResult(cert, setParamSvc, setParamArg);

		// now we try to start the execution of the activity
		StartActivityExecutionService svc = new StartActivityExecutionService();
		LocatorArgument arg = new LocatorArgument();
		arg.realm = "execution";
		arg.locator = activityLoc;

		doServiceAssertResult(cert, svc, arg);

		// and verify that the activity is still created as the 
		// first element tries to reserve the machine, which fail
		// as we previously reserved it manually
		try (StrolchTransaction tx = getRealm("execution").openTx(cert, ReservationExecutionTest.class, true)) {
			Action action = tx.findElement(activityLoc.append("produce"));
			assertEquals(State.CREATED, action.getState());
		}
	}

	@Test
	public void shouldExecuteWhenNotReserved() throws InterruptedException {

		Locator activityLoc = Locator.valueOf(Tags.ACTIVITY, "ToStock", "produceMachine2");

		Certificate cert = loginTest();

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, ReservationExecutionTest.class, true)) {

			// verify that the machine is not reserved
			Resource machine = tx.getResourceBy("Machine", "machine1");
			BooleanParameter reservedP = machine.getParameter(BAG_PARAMETERS, PARAM_RESERVED);
			assertFalse(reservedP.getValue());
		}

		// start the execution of the activity
		StartActivityExecutionService svc = new StartActivityExecutionService();
		LocatorArgument arg = new LocatorArgument();
		arg.realm = "execution";
		arg.locator = activityLoc;

		doServiceAssertResult(cert, svc, arg);

		Thread.sleep(50L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, ReservationExecutionTest.class, true)) {

			// and verify that the activity is in execution
			Action action = tx.findElement(activityLoc.append("reserve"));
			assertEquals(State.EXECUTED, action.getState());

			// verify that the machine is reserved
			Resource machine = tx.getResourceBy("Machine", "machine1");
			BooleanParameter reservedP = machine.getParameter(BAG_PARAMETERS, PARAM_RESERVED);
			assertTrue(reservedP.getValue());
		}

		// now wait till execution should be finished
		Thread.sleep(300L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, ReservationExecutionTest.class, true)) {

			// and now verify is executed
			Activity activity = tx.findElement(activityLoc);
			assertEquals(State.EXECUTED, activity.getState());

			// verify that the machine is not reserved anymore
			Resource machine = tx.getResourceBy("Machine", "machine1");
			BooleanParameter reservedP = machine.getParameter(BAG_PARAMETERS, PARAM_RESERVED);
			assertFalse(reservedP.getValue());
		}
	}
}
