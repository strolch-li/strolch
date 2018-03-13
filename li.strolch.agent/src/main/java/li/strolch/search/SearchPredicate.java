package li.strolch.search;

public interface SearchPredicate {

	boolean matches(Object value);

	default SearchPredicate not() {
		return element -> !this.matches(element);
	}
}
