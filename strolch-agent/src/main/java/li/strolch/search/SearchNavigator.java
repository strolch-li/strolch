package li.strolch.search;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;

import java.util.stream.Stream;

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
