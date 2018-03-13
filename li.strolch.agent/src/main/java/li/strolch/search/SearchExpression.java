package li.strolch.search;

import li.strolch.model.StrolchRootElement;

public interface SearchExpression {

	boolean matches(StrolchRootElement element);

	default SearchExpression or(SearchExpression right) {
		return element -> this.matches(element) || right.matches(element);
	}

	default SearchExpression and(SearchExpression right) {
		return element -> this.matches(element) && right.matches(element);
	}

	default SearchExpression not() {
		return element -> !this.matches(element);
	}
}
