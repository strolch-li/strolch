package li.strolch.search;

import java.util.stream.Stream;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;

public interface SearchNavigator {

	Stream<? extends StrolchRootElement> navigate(StrolchTransaction tx);
}
