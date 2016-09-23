package li.strolch.service.execution;

import static li.strolch.utils.helper.StringHelper.DASH;

import java.util.Iterator;
import java.util.Map.Entry;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.exception.StrolchException;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.activity.TimeOrderingVisitor;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.visitor.IActivityElementVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.service.api.Command;
import li.strolch.utils.helper.StringHelper;

public abstract class ExecutionCommand extends Command implements TimeOrderingVisitor, IActivityElementVisitor {

	public ExecutionCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public static Resource getResource(StrolchTransaction tx, Action action) {
		String resourceId = action.getResourceId();
		if (StringHelper.isEmpty(resourceId) || resourceId.equals(DASH))
			throw new StrolchException("No resourceId defined on action " + action.getLocator());
		String resourceType = action.getResourceType();
		if (StringHelper.isEmpty(resourceType) || resourceType.equals(DASH))
			throw new StrolchException("No resourceType defined on action " + action.getLocator());

		Resource resource = tx.getResourceBy(resourceType, resourceId, true);
		return resource;
	}

	protected ExecutionPolicy getExecutionPolicy(Action action) {
		Resource resource = getResource(tx(), action);
		PolicyDef executionPolicyDef = resource.getPolicyDefs().getPolicyDef(ExecutionPolicy.class.getSimpleName());
		return getComponent(PolicyHandler.class).getPolicy(executionPolicyDef, tx());
	}

	@Override
	public void visitSeries(Activity activity) {

		if (activity.getState() == State.EXECUTED)
			return;

		Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement element = iter.next().getValue();

			boolean canExecute = element.getState().compareTo(State.EXECUTION) < 0;
			if (canExecute) {
				element.accept(this);
				break;
			}
		}
	}

	@Override
	public void visitParallel(Activity activity) {

		if (activity.getState() == State.EXECUTED)
			return;

		Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement element = iter.next().getValue();

			boolean canExecute = element.getState().compareTo(State.EXECUTION) < 0;
			if (canExecute) {
				element.accept(this);
			}
		}
	}

	@Override
	public void visit(Activity activity) {
		activity.getTimeOrdering().accept(this, activity);
	}

	@Override
	public void visit(Action action) {
		ExecutionPolicy executionPolicy = getExecutionPolicy(action);
		executionPolicy.execute(action);
	}
}