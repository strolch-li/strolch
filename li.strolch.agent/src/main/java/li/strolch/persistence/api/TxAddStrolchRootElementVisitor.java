package li.strolch.persistence.api;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchRootElementVisitor;

public class TxAddStrolchRootElementVisitor implements StrolchRootElementVisitor<Void> {

	private final StrolchTransaction tx;

	public TxAddStrolchRootElementVisitor(StrolchTransaction tx) {
		this.tx = tx;
	}

	@Override
	public Void visitOrder(Order order) {
		tx.add(order);
		return null;
	}

	@Override
	public Void visitResource(Resource resource) {
		tx.add(resource);
		return null;
	}

	@Override
	public Void visitActivity(Activity activity) {
		tx.add(activity);
		return null;
	}
}
