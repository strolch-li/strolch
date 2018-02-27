package li.strolch.persistence.api;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchRootElementVisitor;

public class TxRemoveStrolchRootElementVisitor implements StrolchRootElementVisitor<Void> {

	private final StrolchTransaction tx;

	public TxRemoveStrolchRootElementVisitor(StrolchTransaction tx) {
		this.tx = tx;
	}

	@Override
	public Void visitOrder(Order order) {
		tx.remove(order);
		return null;
	}

	@Override
	public Void visitResource(Resource resource) {
		tx.remove(resource);
		return null;
	}

	@Override
	public Void visitActivity(Activity activity) {
		tx.remove(activity);
		return null;
	}
}
