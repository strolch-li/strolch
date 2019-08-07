package li.strolch.model;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

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
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	<U, T extends Parameter<U>> T getParameter(String bagKey, String paramKey);

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the given bagKey, or null if
	 * the {@link Parameter} or the {@link ParameterBag} does not exist
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	<U, T extends Parameter<U>> T getParameter(String bagKey, String paramKey, boolean assertExists);

	/**
	 * Returns a {@link Stream} of all the {@link Parameter Parameters} for the given {@link ParameterBag}
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter Parameters} are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	Stream<Parameter<?>> streamOfParameters(String bagKey);

	/**
	 * Returns a {@link Stream} of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	Stream<Parameter<?>> streamOfParametersByInterpretation(String bagKey, String interpretation);

	/**
	 * Returns a {@link Stream} of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 * @param uom
	 * 		the uom for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	Stream<Parameter<?>> streamOfParametersByInterpretationAndUom(String bagKey, String interpretation, String uom);

	/**
	 * Returns a list of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	List<Parameter<?>> getParametersByInterpretation(String bagKey, String interpretation);

	/**
	 * Returns a list of all the {@link Parameter Parameters} with the given interpretation
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be returned
	 * @param interpretation
	 * 		the interpretation for which the parameters are to be returned
	 * @param uom
	 * 		the uom for which the parameters are to be returned
	 *
	 * @return the parameters with the given interpretation
	 */
	List<Parameter<?>> getParametersByInterpretationAndUom(String bagKey, String interpretation, String uom);

	/**
	 * Adds a new {@link Parameter} to the {@link ParameterBag} with the given key
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} to which the {@link Parameter} should be added
	 * @param parameter
	 * 		the {@link Parameter} to be added to the {@link ParameterBag}
	 *
	 * @throws StrolchException
	 * 		if the {@link ParameterBag} does not exist
	 */
	void addParameter(String bagKey, Parameter<?> parameter) throws StrolchException;

	/**
	 * Removes the {@link Parameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} from which the {@link Parameter} is to be removed
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be removed
	 *
	 * @return the removed {@link Parameter} or null if it did not exist
	 */
	<U, T extends Parameter<U>> T removeParameter(String bagKey, String paramKey);

	/**
	 * Returns the {@link ParameterBag} with the given key, or null if it does not exist
	 *
	 * @param key
	 * 		the key of the {@link ParameterBag} to return
	 *
	 * @return the {@link ParameterBag} with the given key, or null if it does not exist
	 */
	ParameterBag getParameterBag(String key);

	/**
	 * Returns the {@link ParameterBag} with the given key, or null if it does not exist
	 *
	 * @param key
	 * 		the key of the {@link ParameterBag} to return
	 * @param assertExists
	 * 		if set to true, and the parameter bag does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the {@link ParameterBag} with the given key, or null if it does not exist
	 */
	ParameterBag getParameterBag(String key, boolean assertExists);

	/**
	 * Returns a {@link Stream} of {@link ParameterBag ParameterBags}
	 *
	 * @return the {@link ParameterBag ParameterBags}
	 */
	Stream<ParameterBag> streamOfParameterBags();

	/**
	 * Returns a {@link Stream} of {@link ParameterBag ParameterBags} of the given type
	 *
	 * @param type
	 * 		the type of {@link ParameterBag} to return
	 *
	 * @return the {@link ParameterBag ParameterBags} of the given type
	 */
	Stream<ParameterBag> streamOfParameterBagsByType(String type);

	/**
	 * Returns the {@link ParameterBag ParameterBags} of the given type
	 *
	 * @param type
	 * 		the type of {@link ParameterBag} to return
	 *
	 * @return the {@link ParameterBag ParameterBags} of the given type
	 */
	List<ParameterBag> getParameterBagsByType(String type);

	/**
	 * Adds the given {@link ParameterBag} to this {@link GroupedParameterizedElement}
	 *
	 * @param bag
	 * 		the {@link ParameterBag} to add
	 */
	void addParameterBag(ParameterBag bag);

	/**
	 * Removes the {@link ParameterBag} with the given key
	 *
	 * @param key
	 * 		the key of the {@link ParameterBag} to remove
	 *
	 * @return the removed {@link ParameterBag}, or null if it does not exist
	 */
	ParameterBag removeParameterBag(String key);

	/**
	 * Returns true if this {@link GroupedParameterizedElement} has any {@link ParameterBag ParameterBag}
	 *
	 * @return true if this {@link GroupedParameterizedElement} has any {@link ParameterBag ParameterBag}
	 */
	boolean hasParameterBags();

	/**
	 * Returns true if the {@link ParameterBag} with the given key exists on this {@link GroupedParameterizedElement}.
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} which is to be checked for existence
	 *
	 * @return true if the {@link ParameterBag} with the given key exists on this {@link GroupedParameterizedElement}.
	 */
	boolean hasParameterBag(String bagKey);

	/**
	 * Returns true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 * bagKey
	 *
	 * @param bagKey
	 * 		the key of the {@link ParameterBag} on which to find the {@link Parameter}
	 * @param paramKey
	 * 		the key of the {@link Parameter} to be found
	 *
	 * @return true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 * bagKey. False is returned if the {@link ParameterBag} does not exist, or the {@link Parameter} does not exist on
	 * the {@link ParameterBag}
	 */
	boolean hasParameter(String bagKey, String paramKey);

	/**
	 * Returns the {@link Set} of keys for the {@link ParameterBag}s on this {@link GroupedParameterizedElement}
	 *
	 * @return the {@link Set} of keys for the {@link ParameterBag}s on this {@link GroupedParameterizedElement}
	 */
	Set<String> getParameterBagKeySet();
}
