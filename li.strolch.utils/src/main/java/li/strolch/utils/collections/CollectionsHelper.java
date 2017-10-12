package li.strolch.utils.collections;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class CollectionsHelper {

	public static <T> Collector<T, List<T>, T> singletonCollector() {
		return singletonCollector(null);
	}

	public static <T> Collector<T, List<T>, T> singletonCollector(String errorMsg) {
		return Collector.of(ArrayList::new, List::add, (left, right) -> {
			left.addAll(right);
			return left;
		}, list -> {

			if (list.isEmpty()) {
				if (errorMsg == null) {
					throw new IllegalStateException("Expect one element, but received no elements");
				} else
					throw new IllegalStateException(errorMsg);
			}

			if (list.size() != 1) {
				String listS = list.stream().map(Object::toString).collect(Collectors.joining(", "));
				if (errorMsg == null) {
					throw new IllegalStateException(
							"Expect one element, but received " + list.size() + " in list " + listS);
				} else
					throw new IllegalStateException(errorMsg + ": " + listS);
			}

			return list.get(0);
		});
	}
}
