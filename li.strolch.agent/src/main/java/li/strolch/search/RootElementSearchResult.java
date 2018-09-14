package li.strolch.search;

import java.util.Comparator;
import java.util.stream.Stream;

import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.visitor.StrolchRootElementVisitor;

/**
 * A search result for {@link StrolchSearch} for {@link StrolchRootElement} adding methods specific to root element
 *
 * @param <T>
 */
public class RootElementSearchResult<T extends StrolchRootElement> extends SearchResult<T> {

	public RootElementSearchResult(Stream<T> stream) {
		super(stream);
	}

	/**
	 * Appends a comparator to the stream of elements to compare by ID
	 *
	 * @param reversed
	 * 		flag to reverse the comparison
	 *
	 * @return this for chaining
	 */
	public RootElementSearchResult<T> orderById(boolean reversed) {
		Comparator<T> comparator = Comparator.comparing(StrolchElement::getId);
		if (reversed)
			comparator = comparator.reversed();
		this.stream = this.stream.sorted(comparator);
		return this;
	}

	/**
	 * Appends a comparator to the stream of elements to compare by name
	 *
	 * @param reversed
	 * 		flag to reverse the comparison
	 *
	 * @return this for chaining
	 */
	public RootElementSearchResult<T> orderByName(boolean reversed) {
		Comparator<T> comparator = Comparator.comparing(t -> t.getName().toLowerCase());
		if (reversed)
			comparator = comparator.reversed();
		this.stream = this.stream.sorted(comparator);
		return this;
	}

	/**
	 * Appends a comparator to the stream of elements to compare by a parameter
	 *
	 * @param bagId
	 * 		the ID of the bag where the parameter is to be found
	 * @param paramId
	 * 		the ID of the parameter to use for comparing
	 * @param reversed
	 * 		flag to reverse the comparison
	 *
	 * @return this for chaining
	 */
	public RootElementSearchResult<T> orderByParam(String bagId, String paramId, boolean reversed) {
		Comparator<T> comparator = (o1, o2) -> {
			Parameter<?> param1 = o1.getParameter(bagId, paramId);
			Parameter<?> param2 = o2.getParameter(bagId, paramId);
			if (param1 == null && param2 == null)
				return 0;
			if (param1 == null)
				return -1;
			if (param2 == null)
				return 1;
			return param1.compareTo(param2);
		};
		if (reversed)
			comparator = comparator.reversed();
		this.stream = this.stream.sorted(comparator);
		return this;
	}

	/**
	 * <p>Appends a map to the stream which clones the elements in the stream with their version by calling {@link
	 * StrolchRootElement#getClone(boolean)}</p>
	 *
	 * <p>Use this method if you know you are going to modify the elements in the search result.</p>
	 *
	 * @return this instance for chaining.
	 */
	public RootElementSearchResult<T> cloneIfReadOnly() {
		this.stream = this.stream.map(e -> {
			if (!e.isReadOnly())
				return e;
			@SuppressWarnings("unchecked")
			T clone = (T) e.getClone(true);
			return clone;

		});
		return this;
	}

	/**
	 * Transforms this search result to be a search result returning elements with the given visitor
	 *
	 * @param visitor
	 * 		the visitor to transform this search result's elements
	 * @param <U>
	 * 		the type of element to transform to
	 *
	 * @return the new search result for chaining
	 */
	public <U> SearchResult<U> visitor(StrolchRootElementVisitor<U> visitor) {
		return new SearchResult<U>(this.stream.map(e -> e.accept(visitor)));
	}
}
