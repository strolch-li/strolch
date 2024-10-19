package li.strolch.search;

import li.strolch.model.Order;

import java.util.Comparator;
import java.util.stream.Stream;

public class OrderSearchResult extends RootElementSearchResult<Order> {
	public OrderSearchResult(Stream<Order> stream) {
		super(stream);
	}

	/**
	 * Appends a comparator to the stream of elements to compare by date
	 *
	 * @return this for chaining
	 */
	public OrderSearchResult orderByDate() {
		return orderByDate(false);
	}

	/**
	 * Appends a comparator to the stream of elements to compare by date
	 *
	 * @param reversed flag to reverse the comparison
	 *
	 * @return this for chaining
	 */
	public OrderSearchResult orderByDate(boolean reversed) {
		Comparator<Order> comparator = Comparator.comparing(Order::getDate);
		if (reversed)
			comparator = comparator.reversed();
		this.stream = this.stream.sorted(comparator);
		return this;
	}
}
