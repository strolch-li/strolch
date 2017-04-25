package li.strolch.model;

import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.parameter.Parameter;

/**
 * A {@link ParameterBagContainer} has a map of {@link ParameterBag ParameterBags} where the key is the id of the
 * parameter bag. This allows to group {@link Parameter Parameters} in strolch objects
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ParameterBagContainer extends StrolchElement {

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the given bagKey, or null if
	 * the {@link Parameter} or the {@link ParameterBag} does not exist
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param paramKey
	 *            the key of the {@link Parameter} which is to be returned
	 * 
	 * @return the found {@link Parameter} or null if it was not found
	 */
	public <U, T extends Parameter<U>> T getParameter(String bagKey, String paramKey);

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the given bagKey, or null if
	 * the {@link Parameter} or the {@link ParameterBag} does not exist
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param paramKey
	 *            the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 *            if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 * 
	 * @return the found {@link Parameter} or null if it was not found
	 */
	public <U, T extends Parameter<U>> T getParameter(String bagKey, String paramKey, boolean assertExists);

	/**
	 * Adds a new {@link Parameter} to the {@link ParameterBag} with the given key
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} to which the {@link Parameter} should be added
	 * @param parameter
	 *            the {@link Parameter} to be added to the {@link ParameterBag}
	 * 
	 * @throws StrolchException
	 *             if the {@link ParameterBag} does not exist
	 */
	public void addParameter(String bagKey, Parameter<?> parameter) throws StrolchException;

	/**
	 * Removes the {@link Parameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} from which the {@link Parameter} is to be removed
	 * @param paramKey
	 *            the key of the {@link Parameter} which is to be removed
	 * 
	 * @return the removed {@link Parameter} or null if it did not exist
	 */
	public <T> Parameter<T> removeParameter(String bagKey, String paramKey);

	/**
	 * Returns the {@link ParameterBag} with the given key, or null if it does not exist
	 * 
	 * @param key
	 *            the key of the {@link ParameterBag} to return
	 * 
	 * @return the {@link ParameterBag} with the given key, or null if it does not exist
	 */
	public ParameterBag getParameterBag(String key);

	/**
	 * Adds the given {@link ParameterBag} to this {@link GroupedParameterizedElement}
	 * 
	 * @param bag
	 *            the {@link ParameterBag} to add
	 */
	public void addParameterBag(ParameterBag bag);

	/**
	 * Removes the {@link ParameterBag} with the given key
	 * 
	 * @param key
	 *            the key of the {@link ParameterBag} to remove
	 * 
	 * @return the removed {@link ParameterBag}, or null if it does not exist
	 */
	public ParameterBag removeParameterBag(String key);

	/**
	 * Returns true if this {@link GroupedParameterizedElement} has any {@link ParameterBag ParameterBag}
	 * 
	 * @return true if this {@link GroupedParameterizedElement} has any {@link ParameterBag ParameterBag}
	 */
	public boolean hasParameterBags();

	/**
	 * Returns true if the {@link ParameterBag} with the given key exists on this {@link GroupedParameterizedElement}.
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} which is to be checked for existence
	 * @return true if the {@link ParameterBag} with the given key exists on this {@link GroupedParameterizedElement}.
	 */
	public boolean hasParameterBag(String bagKey);

	/**
	 * Returns true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 * bagKey
	 * 
	 * @param bagKey
	 *            the key of the {@link ParameterBag} on which to find the {@link Parameter}
	 * @param paramKey
	 *            the key of the {@link Parameter} to be found
	 * 
	 * @return true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 *         bagKey. False is returned if the {@link ParameterBag} does not exist, or the {@link Parameter} does not
	 *         exist on the {@link ParameterBag}
	 */
	public boolean hasParameter(String bagKey, String paramKey);

	/**
	 * Returns the {@link Set} of keys for the {@link ParameterBag}s on this {@link GroupedParameterizedElement}
	 * 
	 * @return the {@link Set} of keys for the {@link ParameterBag}s on this {@link GroupedParameterizedElement}
	 */
	public Set<String> getParameterBagKeySet();
}
