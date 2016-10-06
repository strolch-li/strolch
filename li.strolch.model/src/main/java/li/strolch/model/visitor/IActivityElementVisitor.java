package li.strolch.model.visitor;

import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;

public interface IActivityElementVisitor<T> {

	public T visit(Activity activity);

	public T visit(Action action);
}
