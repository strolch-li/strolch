package li.strolch.model.visitor;

import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;

public interface IActivityElementVisitor {

	public void visit(Activity activity);

	public void visit(Action action);
}
