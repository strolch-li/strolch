package li.strolch.search;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;

public interface SearchExpression<T extends StrolchRootElement> {

	boolean matches(T element);

	default SearchExpression<T> or(SearchExpression<T> right) {
		return element -> this.matches(element) || right.matches(element);
	}

	default SearchExpression<T> and(SearchExpression<T> right) {
		return element -> this.matches(element) && right.matches(element);
	}

	default SearchExpression<T> not() {
		return element -> !this.matches(element);
	}

	default SearchExpression<Resource> asResourceExp() {
		@SuppressWarnings("unchecked")
		SearchExpression<Resource> exp = element -> this.matches((T) element);
		return exp;
	}

	default SearchExpression<Order> asOrderExp() {
		@SuppressWarnings("unchecked")
		SearchExpression<Order> exp = element -> this.matches((T) element);
		return exp;
	}

	default SearchExpression<Activity> asActivityExp() {
		@SuppressWarnings("unchecked")
		SearchExpression<Activity> exp = element -> this.matches((T) element);
		return exp;
	}
}
