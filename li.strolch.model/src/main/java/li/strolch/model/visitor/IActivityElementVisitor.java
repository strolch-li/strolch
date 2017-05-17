package li.strolch.model.visitor;

import li.strolch.model.Order;
import li.strolch.model.Resource;

public interface IActivityElementVisitor<T> extends StrolchElementVisitor<T> {

	@Override
	public default T visitOrder(Order order) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + order.getClass());
	}

	@Override
	public default T visitResource(Resource resource) {
		throw new UnsupportedOperationException(getClass().getName() + " can not handle " + resource.getClass());
	}
}
