package li.strolch.search;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import li.strolch.utils.collections.CollectionsHelper;
import li.strolch.utils.collections.Paging;

/**
 * A search result for {@link StrolchSearch}. Internally a stream is stored and this class provides methods for
 * manipulating the stream
 *
 * @param <T>
 */
public class SearchResult<T> {

	protected Stream<T> stream;

	public SearchResult(Stream<T> stream) {
		this.stream = stream;
	}

	/**
	 * Returns the internal stream
	 *
	 * @return the internal stream
	 */
	public Stream<T> asStream() {
		return this.stream;
	}

	/**
	 * Returns a new search result converting the elements with the given mapper
	 *
	 * @param mapper
	 * 		the function to map the elements
	 * @param <U>
	 * 		the new element type
	 *
	 * @return the new search result
	 */
	public <U> SearchResult<U> map(Function<T, U> mapper) {
		return new SearchResult<>(this.stream.map(mapper));
	}

	/**
	 * Appends a filter to the internal stream
	 *
	 * @param predicate
	 * 		the predicate to filter the elements
	 *
	 * @return this for chaining
	 */
	public SearchResult<T> filter(Predicate<T> predicate) {
		this.stream = this.stream.filter(predicate);
		return this;
	}

	/**
	 * appends a comparator to this stream
	 *
	 * @param comparator
	 * 		the comparator to append to the stream
	 *
	 * @return this for chaining
	 */
	public SearchResult<T> orderBy(Comparator<? super T> comparator) {
		this.stream = this.stream.sorted(comparator);
		return this;
	}

	/**
	 * Collects this stream to a {@link List}
	 *
	 * @return a list of this stream
	 */
	public List<T> toList() {
		return this.stream.collect(Collectors.toList());
	}

	/**
	 * Collects this stream to a {@link Set}
	 *
	 * @return a set of this stream
	 */
	public Set<T> toSet() {
		return this.stream.collect(Collectors.toSet());
	}

	/**
	 * Collects this stream to a {@link Map}, using the given key mapper. The value is returned as is
	 *
	 * @param keyMapper
	 * 		function to get the key of the element
	 *
	 * @return a map of this stream
	 */
	public <U> Map<U, T> toMap(Function<T, U> keyMapper) {
		return this.stream.collect(Collectors.toMap(keyMapper, t -> t));
	}

	/**
	 * Collects this stream to a {@link Map}
	 *
	 * @param keyMapper
	 * 		function to get the key of the element
	 * @param valueMapper
	 * 		function to get the value of the element
	 *
	 * @return a map of this stream
	 */
	public <U, V> Map<U, V> toMap(Function<T, U> keyMapper, Function<T, V> valueMapper) {
		return this.stream.collect(Collectors.toMap(keyMapper, valueMapper));
	}

	/**
	 * Returns a {@link Paging} element to use this object in paged results
	 *
	 * @param offset
	 * 		the element offset
	 * @param limit
	 * 		the limit per page
	 *
	 * @return the paging
	 */
	public Paging<T> toPaging(int offset, int limit) {
		return Paging.asPage(this.stream.collect(Collectors.toList()), offset, limit);
	}

	/**
	 * Returns the single element in the stream, or throws an {@link IllegalStateException} if the stream does not
	 * contain 1 and only 1 element
	 *
	 * @return the single element in the stream
	 *
	 * @throws IllegalStateException
	 * 		if not 1 and only 1 element is in the stream
	 */
	public T toSingleton(Supplier<String> errorMsgSupplier) throws IllegalStateException {
		return this.stream.collect(CollectionsHelper.singletonCollector(errorMsgSupplier));
	}

	/**
	 * Performs a simple for each on every element
	 *
	 * @param consumer
	 * 		the action to perform on each element
	 */
	public void forEach(Consumer<T> consumer) {
		this.stream.forEach(consumer);
	}
}
