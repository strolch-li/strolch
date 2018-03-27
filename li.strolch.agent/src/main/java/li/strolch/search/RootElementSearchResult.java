package li.strolch.search;

import java.util.Comparator;
import java.util.stream.Stream;

import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.visitor.StrolchRootElementVisitor;

public class RootElementSearchResult<T extends StrolchRootElement> extends SearchResult<T> {

	public RootElementSearchResult(Stream<T> stream) {
		super(stream);
	}

	public RootElementSearchResult<T> orderById(boolean reversed) {
		Comparator<T> comparator = Comparator.comparing(StrolchElement::getId);
		if (reversed)
			comparator = comparator.reversed();
		this.stream = this.stream.sorted(comparator);
		return this;
	}

	public RootElementSearchResult<T> orderByName(boolean reversed) {
		Comparator<T> comparator = Comparator.comparing(StrolchElement::getName);
		if (reversed)
			comparator = comparator.reversed();
		this.stream = this.stream.sorted(comparator);
		return this;
	}

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

	public <U> SearchResult<U> visitor(StrolchRootElementVisitor<U> visitor) {
		return new SearchResult<U>(this.stream.map(e -> e.accept(visitor)));
	}
}
