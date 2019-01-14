package li.strolch.search;

import java.util.Collection;
import java.util.stream.Stream;

public class ValueSearch<T> implements SearchPredicates {

	private ValueSearchExpression<T> expression;

	public ValueSearch<T> where(ValueSearchExpression<T> expression) {
		if (this.expression == null)
			this.expression = expression;
		else
			this.expression = this.expression.and(expression);
		return this;
	}

	/**
	 * Performs the actual search on the given input list
	 *
	 * @return the search result
	 */
	public SearchResult<T> search(Collection<T> input) {

		Stream<T> stream = input.stream();

		if (this.expression != null)
			stream = stream.filter(e -> this.expression.matches(e));

		return new SearchResult<>(stream);
	}
}
