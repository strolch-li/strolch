package li.strolch.search;

import static li.strolch.utils.collections.CollectionsHelper.singletonCollector;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import li.strolch.utils.collections.MapOfLists;
import li.strolch.utils.collections.MapOfMaps;
import li.strolch.utils.collections.MapOfSets;
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
	 * Collects this stream to a {@link List}
	 *
	 * @return a list of this stream
	 */
	public JsonArray toJsonArray(Function<T, JsonElement> jsonMapper) {
		return this.stream.map(jsonMapper).collect(JsonArray::new, JsonArray::add, JsonArray::addAll);
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
	 * Collects this stream to a {@link MapOfSets}, using the given key mapper. The value is returned as is
	 *
	 * @param keyMapper
	 * 		function to get the key of the element
	 *
	 * @return a map of this stream
	 */
	public <U> MapOfSets<U, T> toMapOfSets(Function<T, U> keyMapper) {
		return this.stream.collect(MapOfSets::new, //
				(map, e) -> map.addElement(keyMapper.apply(e), e), //
				MapOfSets::addAll);
	}

	/**
	 * Collects this stream to a {@link MapOfSets}, using the given key mapper
	 *
	 * @param keyMapper
	 * 		function to get the key of the element
	 * @param valueMapper
	 * 		function to get the value of the element
	 *
	 * @return a map of this stream
	 */
	public <U, V> MapOfSets<U, V> toMapOfSets(Function<T, U> keyMapper, Function<T, V> valueMapper) {
		return this.stream.collect(MapOfSets::new, //
				(map, e) -> map.addElement(keyMapper.apply(e), valueMapper.apply(e)), //
				MapOfSets::addAll);
	}

	/**
	 * Collects this stream to a {@link MapOfLists}, using the given key mapper. The value is returned as is
	 *
	 * @param keyMapper
	 * 		function to get the key of the element
	 *
	 * @return a map of this stream
	 */
	public <U> MapOfLists<U, T> toMapOfLists(Function<T, U> keyMapper) {
		return this.stream.collect(MapOfLists::new, //
				(map, e) -> map.addElement(keyMapper.apply(e), e), //
				MapOfLists::addAll);
	}

	/**
	 * Collects this stream to a {@link MapOfLists}, using the given key mapper
	 *
	 * @param keyMapper
	 * 		function to get the key of the element
	 * @param valueMapper
	 * 		function to get the value of the element
	 *
	 * @return a map of this stream
	 */
	public <U, V> MapOfLists<U, V> toMapOfLists(Function<T, U> keyMapper, Function<T, V> valueMapper) {
		return this.stream.collect(MapOfLists::new, //
				(map, e) -> map.addElement(keyMapper.apply(e), valueMapper.apply(e)), //
				MapOfLists::addAll);
	}

	/**
	 * Collects this stream to a {@link MapOfMaps}, using the given key mapper and sub key mapper. The value is returned
	 * as is
	 *
	 * @param keyMapper
	 * 		function to get the key of the element
	 * @param subKeyMapper
	 * 		function to get the sub key of the element
	 *
	 * @return a map of this stream
	 */
	public <U, V> MapOfMaps<U, V, T> toMapOfMaps(Function<T, U> keyMapper, Function<T, V> subKeyMapper) {
		return this.stream.collect(MapOfMaps::new, //
				(map, e) -> map.addElement(keyMapper.apply(e), subKeyMapper.apply(e), e), //
				MapOfMaps::putAll);
	}

	/**
	 * Collects this stream to a {@link MapOfLists}, using the given key mapper
	 *
	 * @param keyMapper
	 * 		function to get the key of the element
	 * @param subKeyMapper
	 * 		function to get the sub key of the element
	 * @param valueMapper
	 * 		function to get the value of the element
	 *
	 * @return a map of this stream
	 */
	public <R, U, V> MapOfMaps<U, V, R> toMapOfMaps(Function<T, U> keyMapper, Function<T, V> subKeyMapper,
			Function<T, R> valueMapper) {
		return this.stream.collect(MapOfMaps::new, //
				(map, e) -> map.addElement(keyMapper.apply(e), subKeyMapper.apply(e), valueMapper.apply(e)), //
				MapOfMaps::putAll);
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
	 * Returns a {@link Paging} element to use this object in paged results
	 *
	 * @param offset
	 * 		the element offset
	 * @param limit
	 * 		the limit per page
	 * @param dataSetSize
	 * 		The number of items before filtering
	 *
	 * @return the paging
	 */
	public Paging<T> toPaging(int offset, int limit, long dataSetSize) {
		return Paging.asPage(this.stream.collect(Collectors.toList()), offset, limit, dataSetSize);
	}

	/**
	 * Returns the single element in the stream, or throws an {@link IllegalStateException} if the stream contains more
	 * than 1 element, or the empty {@link Optional}
	 *
	 * @return the single element in the stream
	 *
	 * @throws IllegalStateException
	 * 		if there is more than 1 element in the stream
	 */
	public Optional<T> toSingletonO() {
		return Optional.ofNullable(this.stream.collect(singletonCollector(true)));
	}

	/**
	 * Returns the single element in the stream, or throws an {@link IllegalStateException} if the stream contains more
	 * * than 1 element, or the empty {@link Optional}
	 *
	 * @param errorMsgSupplier
	 * 		the supplier for an error message to use if not 1 and only 1 element is in the collection
	 *
	 * @return the single element in the stream
	 *
	 * @throws IllegalStateException
	 * 		if there is more than 1 element in the stream
	 */
	public Optional<T> toSingletonO(Supplier<String> errorMsgSupplier) {
		return Optional.ofNullable(this.stream.collect(singletonCollector(true, errorMsgSupplier)));
	}

	/**
	 * Returns the single element in the stream, or throws an {@link IllegalStateException} if the stream does not
	 * contain 1 and only 1 element
	 *
	 * @param errorMsgSupplier
	 * 		the supplier for an error message to use if not 1 and only 1 element is in the collection
	 *
	 * @return the single element in the stream
	 *
	 * @throws IllegalStateException
	 * 		if not 1 and only 1 element is in the stream
	 */
	public T toSingleton(Supplier<String> errorMsgSupplier) throws IllegalStateException {
		return this.stream.collect(singletonCollector(errorMsgSupplier));
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

	/**
	 * <p>Returns true if this search result is empty</p>
	 *
	 * <p><b>Note:</b> This is a terminal operation, and the underlying stream is closed after calling this method</p>
	 *
	 * @return true if this search result is empty
	 */
	public boolean isEmpty() {
		return this.stream.findAny().isEmpty();
	}

	/**
	 * <p>Returns true if this search result is <b>NOT</b> empty</p>
	 *
	 * <p><b>Note:</b> This is a terminal operation, and the underlying stream is closed after calling this method</p>
	 *
	 * @return true if this search result is <b>NOT</b> empty
	 */
	public boolean isNotEmpty() {
		return this.stream.findAny().isPresent();
	}
}
