package li.strolch.search;

import li.strolch.model.StrolchRootElement;

public class GenericSearch<T extends StrolchRootElement> extends StrolchSearch<T> {

	@Override
	protected void define() {
		// ignored
	}

	@SuppressWarnings("unchecked")
	protected GenericSearch<T> resources(String... types) {
		super.resources(types);
		return this;
	}

	@SuppressWarnings("unchecked")
	protected GenericSearch<T> orders(String... types) {
		super.orders(types);
		return this;
	}

	@SuppressWarnings("unchecked")
	protected GenericSearch<T> activities(String... types) {
		super.activities(types);
		return this;
	}

	protected GenericSearch<T> where(SearchExpression expression) {
		super.where(expression);
		return this;
	}
}
