package li.strolch.search;

import java.util.stream.Stream;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * Navigate the TX to a {@link Stream} of {@link StrolchRootElement}
 *
 * @param <T>
 */
public interface SearchNavigator<T extends StrolchRootElement> {

	/**
	 * Navigate the TX to a stream of {@link StrolchRootElement}
	 *
	 * @param tx
	 * 		the TX to navigate
	 *
	 * @return a stream of {@link StrolchRootElement}
	 */
	Stream<T> navigate(StrolchTransaction tx);
}
