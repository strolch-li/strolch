package li.strolch.utils.collections;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * A helper class to handle collections
 */
public class CollectionsHelper {

	/**
	 * Returns true if the elements in the two given lists are equal according to the given comparator regardless of the
	 * sequence
	 *
	 * @param one
	 * 		the first list
	 * @param two
	 * 		the second list
	 * @param comparator
	 * 		the comparator
	 * @param <T>
	 * 		the type of elements being compared
	 *
	 * @return true if the lists have the same elements, regardless of their order in the given lists
	 */
	public static <T> boolean equalsUnordered(List<T> one, List<T> two, Comparator<T> comparator) {
		if (one == null && two == null)
			return true;
		if (one == null || two == null || one.size() != two.size())
			return false;

		// copy lists
		one = new ArrayList<>(one);
		two = new ArrayList<>(two);

		one.sort(comparator);
		two.sort(comparator);

		return one.equals(two);
	}

	/**
	 * Returns a collector which returns exactly one element from a stream, and throws an exception if not exactly one
	 * element is in the stream
	 *
	 * @param <T>
	 * 		the type of element to return
	 *
	 * @return the singleton collector
	 *
	 * @throws IllegalStateException
	 * 		if not 1 and only 1 element is in the stream
	 */
	public static <T> Collector<T, List<T>, T> singletonCollector() throws IllegalStateException {
		return singletonCollector(false, () -> null);
	}

	/**
	 * Returns a collector which returns exactly one element from a stream, and throws an exception if not exactly one
	 * element is in the stream
	 *
	 * @param allowNull
	 * 		if true, then if the stream is empty, null is returned, instead of an exception is thrown
	 * @param <T>
	 * 		the type of element to return
	 *
	 * @return the singleton collector
	 *
	 * @throws IllegalStateException
	 * 		if not 1 and only 1 element is in the stream
	 */
	public static <T> Collector<T, List<T>, T> singletonCollector(boolean allowNull) throws IllegalStateException {
		return singletonCollector(allowNull, () -> null);
	}

	/**
	 * Returns a collector which returns exactly one element from a stream, and throws an exception if not exactly one
	 * element is in the stream
	 *
	 * @param errorMsg
	 * 		the error message to use if not 1 and only 1 element is in the collection
	 * @param <T>
	 * 		the type of element to return
	 *
	 * @return the singleton collector
	 *
	 * @throws IllegalStateException
	 * 		if not 1 and only 1 element is in the stream
	 */
	public static <T> Collector<T, List<T>, T> singletonCollector(String errorMsg) throws IllegalStateException {
		return singletonCollector(false, () -> errorMsg);
	}

	/**
	 * Returns a collector which returns exactly one element from a stream, and throws an exception if not exactly one
	 * element is in the stream
	 *
	 * @param errorMsgSupplier
	 * 		the supplier for an error message to use if not 1 and only 1 element is in the collection
	 * @param <T>
	 * 		the type of element to return
	 *
	 * @return the singleton collector
	 *
	 * @throws IllegalStateException
	 * 		if not 1 and only 1 element is in the stream
	 */
	public static <T> Collector<T, List<T>, T> singletonCollector(Supplier<String> errorMsgSupplier)
			throws IllegalStateException {
		return singletonCollector(false, errorMsgSupplier);
	}

	/**
	 * Returns a collector which returns exactly one element from a stream, and throws an exception if not exactly one
	 * element is in the stream
	 *
	 * @param allowNull
	 * 		if true, then if the stream is empty, null is returned, instead of an exception is thrown
	 * @param errorMsgSupplier
	 * 		the supplier for an error message to use if not 1 and only 1 element is in the collection
	 * @param <T>
	 * 		the type of element to return
	 *
	 * @return the singleton collector
	 *
	 * @throws IllegalStateException
	 * 		if not 1 and only 1 element is in the stream
	 */
	public static <T> Collector<T, List<T>, T> singletonCollector(boolean allowNull, Supplier<String> errorMsgSupplier)
			throws IllegalStateException {
		return Collector.of(ArrayList::new, List::add, (left, right) -> {
			left.addAll(right);
			return left;
		}, list -> {

			if (list.isEmpty()) {
				if (allowNull)
					return null;

				String errorMsg = errorMsgSupplier.get();
				if (errorMsg == null) {
					throw new IllegalStateException("Expect one element, but received no elements");
				} else
					throw new IllegalStateException(errorMsg + ": (none)");
			}

			if (list.size() != 1) {
				String listS = list.stream().map(Object::toString).collect(Collectors.joining(", "));
				String errorMsg = errorMsgSupplier.get();
				if (errorMsg == null) {
					throw new IllegalStateException(
							"Expect one element, but received " + list.size() + " in list " + listS);
				} else
					throw new IllegalStateException(errorMsg + ": " + listS);
			}

			return list.get(0);
		});
	}

	/**
	 * Returns a stream over an array of bytes
	 *
	 * @param bytes
	 * 		the bytes to stream
	 *
	 * @return the stream of bytes
	 */
	public static Stream<Byte> byteStream(byte[] bytes) {
		return StreamSupport.stream(new Spliterators.AbstractSpliterator<>(bytes.length, Spliterator.ORDERED) {
			int pos;

			public boolean tryAdvance(Consumer<? super Byte> action) {
				if (pos < bytes.length) {
					action.accept(bytes[pos++]);
					return true;
				}

				return false;
			}
		}, false);
	}
}
