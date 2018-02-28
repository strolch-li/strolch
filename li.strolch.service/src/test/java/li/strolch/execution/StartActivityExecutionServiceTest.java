package li.strolch.execution;

import static org.junit.Assert.assertEquals;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import li.strolch.execution.service.StartActivityExecutionService;
import li.strolch.model.Locator;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.service.LocatorArgument;
import li.strolch.testbase.runtime.RuntimeMock;

public class StartActivityExecutionServiceTest extends RuntimeMock {

	@Before
	public void before() {
		mockRuntime(new File("target/" + StartActivityExecutionServiceTest.class.getName()),
				new File("src/test/resources/executiontest"));
		startContainer();
	}

	@After
	public void after() {
		destroyRuntime();
	}

	@Test
	public void shouldExecuteSingleActionActivity() throws InterruptedException {

		Locator activityLoc = Locator.valueOf(Tags.ACTIVITY, "ToStock", "produceBicycle");

		Certificate cert = loginTest();

		StartActivityExecutionService svc = new StartActivityExecutionService();
		LocatorArgument arg = new LocatorArgument();
		arg.realm = "execution";
		arg.locator = activityLoc;

		doServiceAssertResult(cert, svc, arg);

		// allow execution handler to do work
		Thread.sleep(100);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action = tx.findElement(activityLoc.append("produce"));
			assertEquals(State.EXECUTION, action.getState());
		}

		Thread.sleep(200L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action = tx.findElement(activityLoc.append("produce"));
			assertEquals(State.EXECUTED, action.getState());
		}
	}

	@Test
	public void shouldExecuteSeriesActivity() throws InterruptedException {

		Locator activityLoc = Locator.valueOf(Tags.ACTIVITY, "ToStock", "conveyors");

		Certificate cert = loginTest();

		StartActivityExecutionService svc = new StartActivityExecutionService();
		LocatorArgument arg = new LocatorArgument();
		arg.realm = "execution";
		arg.locator = activityLoc;

		doServiceAssertResult(cert, svc, arg);

		// allow execution handler to do work
		Thread.sleep(60);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action;

			action = tx.findElement(activityLoc.append("action_1"));
			assertEquals(State.EXECUTION, action.getState());

			action = tx.findElement(activityLoc.append("action_2"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("action_3"));
			assertEquals(State.CREATED, action.getState());
		}

		Thread.sleep(400L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action;

			action = tx.findElement(activityLoc.append("action_1"));
			assertEquals(State.EXECUTED, action.getState());

			action = tx.findElement(activityLoc.append("action_2"));
			assertEquals(State.EXECUTION, action.getState());

			action = tx.findElement(activityLoc.append("action_3"));
			assertEquals(State.CREATED, action.getState());
		}

		Thread.sleep(300L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action;

			action = tx.findElement(activityLoc.append("action_1"));
			assertEquals(State.EXECUTED, action.getState());

			action = tx.findElement(activityLoc.append("action_2"));
			assertEquals(State.EXECUTED, action.getState());

			action = tx.findElement(activityLoc.append("action_3"));
			assertEquals(State.EXECUTION, action.getState());
		}

		Thread.sleep(300L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action;

			action = tx.findElement(activityLoc.append("action_1"));
			assertEquals(State.EXECUTED, action.getState());

			action = tx.findElement(activityLoc.append("action_2"));
			assertEquals(State.EXECUTED, action.getState());

			action = tx.findElement(activityLoc.append("action_3"));
			assertEquals(State.EXECUTED, action.getState());

			Activity activity = (Activity) tx.findElement(activityLoc.append("action_3")).getRootElement();
			assertEquals(State.EXECUTED, activity.getState());

		}
	}

	@Test
	public void shouldExecuteParallelActivity() throws InterruptedException {

		Locator activityLoc = Locator.valueOf(Tags.ACTIVITY, "ToStock", "parallel");

		Certificate cert = loginTest();

		StartActivityExecutionService svc = new StartActivityExecutionService();
		LocatorArgument arg = new LocatorArgument();
		arg.realm = "execution";
		arg.locator = activityLoc;

		doServiceAssertResult(cert, svc, arg);

		// allow execution handler to do work
		Thread.sleep(100);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action;

			// sub1 also parallel
			action = tx.findElement(activityLoc.append("sub1", "action_1"));
			assertEquals(State.EXECUTION, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_2"));
			assertEquals(State.EXECUTION, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_3"));
			assertEquals(State.EXECUTION, action.getState());

			// sub2 in series
			action = tx.findElement(activityLoc.append("sub2", "action_1"));
			assertEquals(State.EXECUTION, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_2"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_3"));
			assertEquals(State.CREATED, action.getState());

			// actions
			action = tx.findElement(activityLoc.append("action_1"));
			assertEquals(State.EXECUTION, action.getState());
			action = tx.findElement(activityLoc.append("action_2"));
			assertEquals(State.EXECUTION, action.getState());
			action = tx.findElement(activityLoc.append("action_3"));
			assertEquals(State.EXECUTION, action.getState());
		}

		Thread.sleep(200L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action;

			// sub1 also parallel
			action = tx.findElement(activityLoc.append("sub1", "action_1"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_2"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_3"));
			assertEquals(State.EXECUTED, action.getState());

			// sub2 in series
			action = tx.findElement(activityLoc.append("sub2", "action_1"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_2"));
			assertEquals(State.EXECUTION, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_3"));
			assertEquals(State.CREATED, action.getState());

			// actions
			action = tx.findElement(activityLoc.append("action_1"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("action_2"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("action_3"));
			assertEquals(State.EXECUTED, action.getState());

			Activity activity = tx.findElement(activityLoc);
			assertEquals(State.EXECUTION, activity.getState());
		}

		Thread.sleep(200L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action;

			// sub1 also parallel
			action = tx.findElement(activityLoc.append("sub1", "action_1"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_2"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_3"));
			assertEquals(State.EXECUTED, action.getState());

			// sub2 in series
			action = tx.findElement(activityLoc.append("sub2", "action_1"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_2"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_3"));
			assertEquals(State.EXECUTION, action.getState());

			// actions
			action = tx.findElement(activityLoc.append("action_1"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("action_2"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("action_3"));
			assertEquals(State.EXECUTED, action.getState());

			Activity activity = tx.findElement(activityLoc);
			assertEquals(State.EXECUTION, activity.getState());
		}

		Thread.sleep(200L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {

			Activity activity = tx.findElement(activityLoc);
			assertEquals(State.EXECUTED, activity.getState());
		}
	}

	@Test
	public void shouldExecuteDeepActivity() throws InterruptedException {

		Locator activityLoc = Locator.valueOf(Tags.ACTIVITY, "ToStock", "deep");

		Certificate cert = loginTest();

		StartActivityExecutionService svc = new StartActivityExecutionService();
		LocatorArgument arg = new LocatorArgument();
		arg.realm = "execution";
		arg.locator = activityLoc;

		doServiceAssertResult(cert, svc, arg);

		// allow execution handler to do work
		Thread.sleep(20);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action;

			// sub1 in series
			action = tx.findElement(activityLoc.append("sub1", "action_1"));
			assertEquals(State.EXECUTION, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_2"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_3"));
			assertEquals(State.CREATED, action.getState());

			// sub2 in series
			action = tx.findElement(activityLoc.append("sub2", "action_1"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_2"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_3"));
			assertEquals(State.CREATED, action.getState());

			// actions
			action = tx.findElement(activityLoc.append("action_1"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("action_2"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("action_3"));
			assertEquals(State.CREATED, action.getState());

			Activity activity = tx.findElement(activityLoc);
			assertEquals(State.EXECUTION, activity.getState());
		}

		Thread.sleep(150L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Action action;

			// sub1 in series
			action = tx.findElement(activityLoc.append("sub1", "action_1"));
			assertEquals(State.EXECUTED, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_2"));
			assertEquals(State.EXECUTION, action.getState());
			action = tx.findElement(activityLoc.append("sub1", "action_3"));
			assertEquals(State.CREATED, action.getState());

			// sub2 in series
			action = tx.findElement(activityLoc.append("sub2", "action_1"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_2"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("sub2", "action_3"));
			assertEquals(State.CREATED, action.getState());

			// actions
			action = tx.findElement(activityLoc.append("action_1"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("action_2"));
			assertEquals(State.CREATED, action.getState());
			action = tx.findElement(activityLoc.append("action_3"));
			assertEquals(State.CREATED, action.getState());

			Activity activity = tx.findElement(activityLoc);
			assertEquals(State.EXECUTION, activity.getState());
		}

		Thread.sleep(1000L);

		try (StrolchTransaction tx = getRealm("execution").openTx(cert, StartActivityExecutionServiceTest.class)) {
			Activity activity = tx.findElement(activityLoc);
			assertEquals(State.EXECUTED, activity.getState());
		}
	}
}
