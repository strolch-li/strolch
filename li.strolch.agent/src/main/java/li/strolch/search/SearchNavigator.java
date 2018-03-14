package li.strolch.search;

import java.util.stream.Stream;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;

public interface SearchNavigator<T extends StrolchRootElement> {

	Stream<T> navigate(StrolchTransaction tx);
}
