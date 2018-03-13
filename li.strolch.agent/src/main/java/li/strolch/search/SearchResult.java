package li.strolch.search;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.utils.collections.Paging;

public class SearchResult<T> {

	protected Stream<T> stream;

	public SearchResult(Stream<T> stream) {
		this.stream = stream;
	}

	public Stream<T> asStream() {
		return this.stream;
	}

	public RootElementSearchResult<Resource> asResources() {
		return new RootElementSearchResult<>(this.stream.map(e -> (Resource) e));
	}

	public RootElementSearchResult<Order> asOrders() {
		return new RootElementSearchResult<>(this.stream.map(e -> (Order) e));
	}

	public RootElementSearchResult<Activity> asActivities() {
		return new RootElementSearchResult<>(this.stream.map(e -> (Activity) e));
	}

	public <U> SearchResult<U> map(Function<T, U> mapper) {
		return new SearchResult<U>(this.stream.map(mapper));
	}

	public SearchResult<T> orderBy(Comparator<? super T> comparator) {
		this.stream = this.stream.sorted(comparator);
		return this;
	}

	public List<T> toList() {
		return this.stream.collect(Collectors.toList());
	}

	public Set<T> toSet() {
		return this.stream.collect(Collectors.toSet());
	}

	public <U, V> Map<U, V> toMap(Function<T, U> keyMapper, Function<T, V> valueMapper) {
		return this.stream.collect(Collectors.toMap(keyMapper, valueMapper));
	}

	public Paging<T> toPaging(int offset, int limit) {
		return Paging.asPage(this.stream.collect(Collectors.toList()), offset, limit);
	}
}
