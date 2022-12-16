package li.strolch.persistence.api;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchRootElementVisitor;

public class TxUpdateStrolchRootElementVisitor implements StrolchRootElementVisitor<Void> {

	private final StrolchTransaction tx;

	public TxUpdateStrolchRootElementVisitor(StrolchTransaction tx) {
		this.tx = tx;
	}

	@Override
	public Void visitOrder(Order order) {
		tx.update(order);
		return null;
	}

	@Override
	public Void visitResource(Resource resource) {
		tx.update(resource);
		return null;
	}

	@Override
	public Void visitActivity(Activity activity) {
		tx.update(activity);
		return null;
	}
}
