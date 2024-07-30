package li.strolch.model;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;
import static li.strolch.model.StrolchModelConstants.*;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.parameter.*;
import li.strolch.utils.iso8601.ISO8601;
import li.strolch.utils.time.PeriodDuration;

/**
 * A {@link ParameterBagContainer} has a map of {@link ParameterBag ParameterBags} where the key is the id of the
 * parameter bag. This allows to group {@link Parameter Parameters} in strolch objects
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ParameterBagContainer extends StrolchElement {

	/**
	 * Returns true if the parameter with the given key on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_PARAMETERS} does not exist or the value is empty, i.e.
	 * {@link Parameter#isEmpty()} returns true
	 *
	 * @param paramKey
	 * 		the parameter to check if it is empty
	 *
	 * @return true if the parameter with the given key on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_PARAMETERS} does not exist or the value is empty, i.e.
	 * {@link Parameter#isEmpty()} returns true
	 */
	default boolean isParamEmpty(String paramKey) {
		return !this.hasParameter(paramKey) || getParameter(paramKey).isEmpty();
	}

	/**
	 * Returns true if the parameter with the given key on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_PARAMETERS} does not exist or the value is empty, i.e.
	 * {@link Parameter#isEmpty()} returns true
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the parameter to check if it is empty
	 *
	 * @return true if the parameter with the given key on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_PARAMETERS} does not exist or the value is empty, i.e.
	 * {@link Parameter#isEmpty()} returns true
	 */
	default boolean isParamEmpty(String bagKey, String paramKey) {
		return !this.hasParameter(bagKey, paramKey) || getParameter(bagKey, paramKey).isEmpty();
	}

	/**
	 * Returns true if the parameter with the given key on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_PARAMETERS} exists and the value is not empty, i.e. {@link Parameter#isSet()}
	 * returns true
	 *
	 * @param paramKey
	 * 		the parameter to check if it has a value
	 *
	 * @return true if the parameter with the given key on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_PARAMETERS} exists and the value is not empty, i.e. {@link Parameter#isSet()}
	 * returns true
	 */
	default boolean isParamSet(String paramKey) {
		return this.hasParameter(paramKey) && getParameter(paramKey).isSet();
	}

	/**
	 * Returns true if the parameter with the given key on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_PARAMETERS} exists and the value is not empty, i.e. {@link Parameter#isSet()}
	 * returns true
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the parameter to check if it has a value
	 *
	 * @return true if the parameter with the given key on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_PARAMETERS} exists and the value is not empty, i.e. {@link Parameter#isSet()}
	 * returns true
	 */
	default boolean isParamSet(String bagKey, String paramKey) {
		return this.hasParameter(bagKey, paramKey) && getParameter(bagKey, paramKey).isSet();
	}

	/**
	 * Returns the value of the {@link StringParameter} with the given paramKey from the {@link ParameterBag} with the
	 * ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default String getString(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? "" : bag.getString(paramKey);
	}

	/**
	 * Returns the value of the {@link StringParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default String getString(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? "" : bag.getString(paramKey);
	}

	/**
	 * Returns the value of the {@link BooleanParameter} with the given paramKey from the {@link ParameterBag} with the
	 * ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default boolean getBoolean(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag != null && bag.getBoolean(paramKey);
	}

	/**
	 * Returns the value of the {@link BooleanParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default boolean getBoolean(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag != null && bag.getBoolean(paramKey);
	}

	/**
	 * Returns the value of the {@link IntegerParameter} with the given paramKey from the {@link ParameterBag} with the
	 * ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default int getInteger(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? 0 : bag.getInteger(paramKey);
	}

	/**
	 * Returns the value of the {@link IntegerParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default int getInteger(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? 0 : bag.getInteger(paramKey);
	}

	/**
	 * Returns the value of the {@link FloatParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default double getDouble(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? 0 : bag.getDouble(paramKey);
	}

	/**
	 * Returns the value of the {@link FloatParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default double getDouble(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag.getDouble(paramKey);
	}

	/**
	 * Returns the value of the {@link LongParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default long getLong(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? 0L : bag.getLong(paramKey);
	}

	/**
	 * Returns the value of the {@link LongParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default long getLong(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? 0L : bag.getLong(paramKey);
	}

	/**
	 * Returns the value of the {@link DateParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default ZonedDateTime getDate(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? ISO8601.EMPTY_VALUE_ZONED_DATE : bag.getDate(paramKey);
	}

	/**
	 * Returns the value of the {@link DateParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default ZonedDateTime getDate(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? ISO8601.EMPTY_VALUE_ZONED_DATE : bag.getDate(paramKey);
	}

	/**
	 * Returns the value of the {@link DateParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default LocalDateTime getLocalDate(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? ISO8601.EMPTY_VALUE_LOCAL_DATE : bag.getLocalDate(paramKey);
	}

	/**
	 * Returns the value of the {@link DateParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default LocalDateTime getLocalDate(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? ISO8601.EMPTY_VALUE_LOCAL_DATE : bag.getLocalDate(paramKey);
	}

	/**
	 * Returns the value of the {@link TextParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default String getText(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? "" : bag.getText(paramKey);
	}

	/**
	 * Returns the value of the {@link TextParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default String getText(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? "" : bag.getText(paramKey);
	}

	/**
	 * Returns the value of the {@link DurationParameter} with the given paramKey from the {@link ParameterBag} with the
	 * ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default PeriodDuration getDuration(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? PeriodDuration.ZERO : bag.getDuration(paramKey);
	}

	/**
	 * Returns the value of the {@link DurationParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default PeriodDuration getDuration(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? PeriodDuration.ZERO : bag.getDuration(paramKey);
	}

	/**
	 * Returns the value of the {@link StringListParameter} with the given paramKey from the {@link ParameterBag} with
	 * the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default List<String> getStringList(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? emptyList() : bag.getStringList(paramKey);
	}

	/**
	 * Returns the value of the {@link StringListParameter} with the given paramKey from the {@link ParameterBag} with
	 * the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default List<String> getStringList(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? emptyList() : bag.getStringList(paramKey);
	}

	/**
	 * Returns the value of the {@link IntegerListParameter} with the given paramKey from the {@link ParameterBag} with
	 * the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default List<Integer> getIntegerList(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? emptyList() : bag.getIntegerList(paramKey);
	}

	/**
	 * Returns the value of the {@link IntegerListParameter} with the given paramKey from the {@link ParameterBag} with
	 * the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default List<Integer> getIntegerList(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? emptyList() : bag.getIntegerList(paramKey);
	}

	/**
	 * Returns the value of the {@link FloatListParameter} with the given paramKey from the {@link ParameterBag} with
	 * the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default List<Double> getDoubleList(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? emptyList() : bag.getDoubleList(paramKey);
	}

	/**
	 * Returns the value of the {@link FloatListParameter} with the given paramKey from the {@link ParameterBag} with
	 * the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default List<Double> getDoubleList(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? emptyList() : bag.getDoubleList(paramKey);
	}

	/**
	 * Returns the value of the {@link LongListParameter} with the given paramKey from the {@link ParameterBag} with the
	 * ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default List<Long> getLongList(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		return bag == null ? emptyList() : bag.getLongList(paramKey);
	}

	/**
	 * Returns the value of the {@link LongListParameter} with the given paramKey from the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 *
	 * @return the value of the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default List<Long> getLongList(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, false);
		return bag == null ? emptyList() : bag.getLongList(paramKey);
	}

	/**
	 * <p>Returns the default {@link ParameterBag}, i.e. the bag with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}</p>
	 *
	 * <p><b>Note:</b> If the bag does not exist, it is created, and added to this container. Thus this element must
	 * not be read-only!</p>
	 *
	 * @return the default {@link ParameterBag}, i.e. the bag with the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 */
	default ParameterBag defaultBag() {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, false);
		if (bag == null) {
			bag = new ParameterBag(BAG_PARAMETERS, TYPE_PARAMETERS, TYPE_PARAMETERS);
			addParameterBag(bag);
		}
		return bag;
	}

	/**
	 * <p>Returns the relations {@link ParameterBag}, i.e. the bag with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}</p>
	 *
	 * <p><b>Note:</b> If the bag does not exist, it is created, and added to this container. Thus this element must
	 * not be read-only!</p>
	 *
	 * @return the relations {@link ParameterBag}, i.e. the bag with the ID {@link StrolchModelConstants#BAG_RELATIONS}
	 */
	default ParameterBag relationsBag() {
		ParameterBag bag = getParameterBag(BAG_RELATIONS, false);
		if (bag == null) {
			bag = new ParameterBag(BAG_RELATIONS, TYPE_RELATIONS, TYPE_RELATIONS);
			addParameterBag(bag);
		}
		return bag;
	}

	/**
	 * Sets the given value on the {@link StringParameter} with the given paramKey on the {@link ParameterBag} with the
	 * ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setString(String paramKey, String value) throws StrolchModelException {
		defaultBag().setString(paramKey, value);
	}

	/**
	 * Sets the given enum's name value on the {@link StringParameter} with the given paramKey on the
	 * {@link ParameterBag} with the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setString(String paramKey, Enum<?> value) throws StrolchModelException {
		defaultBag().setString(paramKey, value.name());
	}

	/**
	 * Sets the given value on the {@link StringParameter} with the given paramKey on the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setString(String bagKey, String paramKey, String value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setString(paramKey, value);
	}

	/**
	 * Sets the given enum's name value on the {@link StringParameter} with the given paramKey on the
	 * {@link ParameterBag} with the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setString(String bagKey, String paramKey, Enum<?> value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setString(paramKey, value.name());
	}

	/**
	 * Sets the given value on the {@link BooleanParameter} with the given paramKey on the {@link ParameterBag} with the
	 * ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setBoolean(String paramKey, boolean value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		BooleanParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new BooleanParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link BooleanParameter} with the given paramKey on the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setBoolean(String bagKey, String paramKey, boolean value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setBoolean(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link IntegerParameter} with the given paramKey on the {@link ParameterBag} with the
	 * ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setInteger(String paramKey, int value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		IntegerParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new IntegerParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link IntegerParameter} with the given paramKey on the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setInteger(String bagKey, String paramKey, int value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setInteger(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link FloatParameter} with the given paramKey on the {@link ParameterBag} with the
	 * ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDouble(String paramKey, double value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		FloatParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new FloatParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link FloatParameter} with the given paramKey on the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDouble(String bagKey, String paramKey, double value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setDouble(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link LongParameter} with the given paramKey on the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setLong(String paramKey, long value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		LongParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new LongParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link LongParameter} with the given paramKey on the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setLong(String bagKey, String paramKey, long value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setLong(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link DateParameter} with the given paramKey on the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDate(String paramKey, ZonedDateTime value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		DateParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValueFromZonedDateTime(value);
		} else {
			bag.addParameter(new DateParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link DateParameter} with the given paramKey on the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDate(String bagKey, String paramKey, ZonedDateTime value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setDate(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link DateParameter} with the given paramKey on the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDate(String paramKey, LocalDateTime value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		DateParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValueFromLocalDateTime(value);
		} else {
			bag.addParameter(new DateParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link DateParameter} with the given paramKey on the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDate(String bagKey, String paramKey, LocalDateTime value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setDate(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link TextParameter} with the given paramKey on the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setText(String paramKey, String value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		TextParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new TextParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link TextParameter} with the given paramKey on the {@link ParameterBag} with the
	 * given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setText(String bagKey, String paramKey, String value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setText(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link DurationParameter} with the given paramKey on the {@link ParameterBag} with
	 * the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDuration(String paramKey, PeriodDuration value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		DurationParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new DurationParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link DurationParameter} with the given paramKey on the {@link ParameterBag} with
	 * the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDuration(String bagKey, String paramKey, PeriodDuration value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setDuration(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link StringListParameter} with the given paramKey on the {@link ParameterBag} with
	 * the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setStringList(String paramKey, List<String> value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		StringListParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new StringListParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link StringListParameter} with the given paramKey on the {@link ParameterBag} with
	 * the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setStringList(String bagKey, String paramKey, List<String> value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setStringList(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link IntegerListParameter} with the given paramKey on the {@link ParameterBag} with
	 * the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setIntegerList(String paramKey, List<Integer> value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		IntegerListParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new IntegerListParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link IntegerListParameter} with the given paramKey on the {@link ParameterBag} with
	 * the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setIntegerList(String bagKey, String paramKey, List<Integer> value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setIntegerList(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link FloatListParameter} with the given paramKey on the {@link ParameterBag} with
	 * the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDoubleList(String paramKey, List<Double> value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		FloatListParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new FloatListParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link FloatListParameter} with the given paramKey on the {@link ParameterBag} with
	 * the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setDoubleList(String bagKey, String paramKey, List<Double> value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setDoubleList(paramKey, value);
	}

	/**
	 * Sets the given value on the {@link LongListParameter} with the given paramKey on the {@link ParameterBag} with
	 * the ID {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setLongList(String paramKey, List<Long> value) throws StrolchModelException {
		ParameterBag bag = defaultBag();
		LongListParameter param = bag.getParameter(paramKey);
		if (param != null) {
			param.setValue(value);
		} else {
			bag.addParameter(new LongListParameter(paramKey, paramKey, value));
		}
	}

	/**
	 * Sets the given value on the {@link LongListParameter} with the given paramKey on the {@link ParameterBag} with
	 * the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter for which to return the value
	 * @param value
	 * 		the value to set on the parameter
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setLongList(String bagKey, String paramKey, List<Long> value) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		bag.setLongList(paramKey, value);
	}

	/**
	 * Returns the {@link StringParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default StringParameter getStringP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getStringP(paramKey);
	}

	/**
	 * Returns the {@link StringParameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default StringParameter getStringP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getStringP(paramKey);
	}

	/**
	 * Returns the {@link BooleanParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default BooleanParameter getBooleanP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getBooleanP(paramKey);
	}

	/**
	 * Returns the {@link BooleanParameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default BooleanParameter getBooleanP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getBooleanP(paramKey);
	}

	/**
	 * Returns the {@link IntegerParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default IntegerParameter getIntegerP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getIntegerP(paramKey);
	}

	/**
	 * Returns the {@link IntegerParameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default IntegerParameter getIntegerP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getIntegerP(paramKey);
	}

	/**
	 * Returns the {@link FloatParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default FloatParameter getDoubleP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getDoubleP(paramKey);
	}

	/**
	 * Returns the {@link FloatParameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default FloatParameter getDoubleP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getDoubleP(paramKey);
	}

	/**
	 * Returns the {@link LongParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default LongParameter getLongP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getLongP(paramKey);
	}

	/**
	 * Returns the {@link LongParameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default LongParameter getLongP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getLongP(paramKey);
	}

	/**
	 * Returns the {@link DateParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default DateParameter getDateP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getDateP(paramKey);
	}

	/**
	 * Returns the {@link DateParameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default DateParameter getDateP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getDateP(paramKey);
	}

	/**
	 * Returns the {@link TextParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default TextParameter getTextP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getTextP(paramKey);
	}

	/**
	 * Returns the {@link TextParameter} with the given paramKey from the {@link ParameterBag} with the given bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default TextParameter getTextP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getTextP(paramKey);
	}

	/**
	 * Returns the {@link DurationParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default DurationParameter getDurationP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getDurationP(paramKey);
	}

	/**
	 * Returns the {@link DurationParameter} with the given paramKey from the {@link ParameterBag} with the given
	 * bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default DurationParameter getDurationP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getDurationP(paramKey);
	}

	/**
	 * Returns the {@link StringListParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default StringListParameter getStringListP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getStringListP(paramKey);
	}

	/**
	 * Returns the {@link StringListParameter} with the given paramKey from the {@link ParameterBag} with the given
	 * bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default StringListParameter getStringListP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getStringListP(paramKey);
	}

	/**
	 * Returns the {@link IntegerListParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default IntegerListParameter getIntegerListP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getIntegerListP(paramKey);
	}

	/**
	 * Returns the {@link IntegerListParameter} with the given paramKey from the {@link ParameterBag} with the given
	 * bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default IntegerListParameter getIntegerListP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getIntegerListP(paramKey);
	}

	/**
	 * Returns the {@link FloatListParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default FloatListParameter getDoubleListP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getDoubleListP(paramKey);
	}

	/**
	 * Returns the {@link FloatListParameter} with the given paramKey from the {@link ParameterBag} with the given
	 * bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default FloatListParameter getDoubleListP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getDoubleListP(paramKey);
	}

	/**
	 * Returns the {@link LongListParameter} with the given paramKey from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default LongListParameter getLongListP(String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(BAG_PARAMETERS, true);
		return bag.getLongListP(paramKey);
	}

	/**
	 * Returns the {@link LongListParameter} with the given paramKey from the {@link ParameterBag} with the given
	 * bagKey
	 *
	 * @param bagKey
	 * 		the key from which {@link ParameterBag} to get the parameter
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default LongListParameter getLongListP(String bagKey, String paramKey) throws StrolchModelException {
		ParameterBag bag = getParameterBag(bagKey, true);
		return bag.getLongListP(paramKey);
	}

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link * StrolchModelConstants#BAG_PARAMETERS}, or null if the {@link Parameter} or the {@link ParameterBag} does
	 * not exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	<U, T extends Parameter<U>> T getParameter(String paramKey);

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_PARAMETERS}, or null if the {@link Parameter} or the {@link ParameterBag} does
	 * not exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	<U, T extends Parameter<U>> T getParameter(String paramKey, boolean assertExists);

	/**
	 * Returns the value of the {@link StringParameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, if the {@link Parameter} or the {@link ParameterBag} does not exist,
	 * then a {@link StrolchModelException} is thrown
	 *
	 * @param paramKey
	 * 		the key of the {@link StringParameter} for which the value is to be returned
	 *
	 * @return the parameter's value
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default String getRelationId(String paramKey) throws StrolchModelException {
		ParameterBag relationsBag = getParameterBag(BAG_RELATIONS, false);
		if (relationsBag == null)
			return "";
		return relationsBag.getString(paramKey);
	}

	/**
	 * Returns the value of the {@link StringListParameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, if the {@link Parameter} or the {@link ParameterBag} does not exist,
	 * then a {@link StrolchModelException} is thrown
	 *
	 * @param paramKey
	 * 		the key of the {@link StringParameter} for which the value is to be returned
	 *
	 * @return the parameter's value
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default List<String> getRelationIds(String paramKey) throws StrolchModelException {
		ParameterBag relationsBag = getParameterBag(BAG_RELATIONS, false);
		if (relationsBag == null)
			return List.of();
		return relationsBag.getStringList(paramKey);
	}

	/**
	 * Returns a stream over the values of the {@link StringListParameter} with the given key from the
	 * {@link ParameterBag} with the ID {@link StrolchModelConstants#BAG_RELATIONS}, if the {@link Parameter} or the
	 * {@link ParameterBag} does not exist, then a {@link StrolchModelException} is thrown
	 *
	 * @param paramKey
	 * 		the key of the {@link StringParameter} for which the value is to be returned
	 *
	 * @return a stream over the the parameter's values
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default Stream<String> streamRelationIds(String paramKey) throws StrolchModelException {
		ParameterBag relationsBag = getParameterBag(BAG_RELATIONS, false);
		if (relationsBag == null)
			return Stream.empty();
		return relationsBag.streamStringList(paramKey);
	}

	/**
	 * Sets the value of the {@link StringParameter} with the given key on the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, if the {@link Parameter} or the {@link ParameterBag} does not exist,
	 * then a {@link StrolchModelException} is thrown
	 *
	 * @param paramKey
	 * 		the key of the {@link StringParameter} for which the value is to be returned
	 * @param id
	 * 		the id of the relation to set
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setRelationId(String paramKey, String id) throws StrolchModelException {
		getRelationParam(paramKey, true).setValue(id);
	}

	/**
	 * Sets the value of the {@link StringListParameter} with the given key on the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, if the {@link Parameter} or the {@link ParameterBag} does not exist,
	 * then a {@link StrolchModelException} is thrown
	 *
	 * @param paramKey
	 * 		the key of the {@link StringParameter} for which the value is to be returned
	 * @param ids
	 * 		the ids of the relation to set
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default void setRelationIds(String paramKey, List<String> ids) throws StrolchModelException {
		getRelationsParam(paramKey, true).setValue(ids);
	}

	/**
	 * Returns the {@link StringParameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, if the {@link Parameter} or the {@link ParameterBag} does not exist,
	 * then a {@link StrolchModelException} is thrown
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter}
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default StringParameter getRelationP(String paramKey) {
		return getParameter(BAG_RELATIONS, paramKey, true);
	}

	/**
	 * Returns the {@link StringParameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does
	 * not exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	default StringParameter getRelationParam(String paramKey) {
		return getParameter(BAG_RELATIONS, paramKey, false);
	}

	/**
	 * Returns the {@link StringParameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does
	 * not exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	default StringParameter getRelationParam(String paramKey, boolean assertExists) {
		return getParameter(BAG_RELATIONS, paramKey, assertExists);
	}

	/**
	 * Returns the {@link StringListParameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does
	 * not exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	default StringListParameter getRelationsParam(String paramKey) {
		return getParameter(BAG_RELATIONS, paramKey, false);
	}

	/**
	 * Returns the {@link StringListParameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, if the {@link Parameter} or the {@link ParameterBag} does not exist,
	 * then a {@link StrolchModelException} is thrown
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter}
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	default StringListParameter getRelationsP(String paramKey) {
		return getParameter(BAG_RELATIONS, paramKey, true);
	}

	/**
	 * Returns the {@link StringListParameter} with the given key from the {@link ParameterBag} with the ID
	 * {@link StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does
	 * not exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	default StringListParameter getRelationsParam(String paramKey, boolean assertExists) {
		return getParameter(BAG_RELATIONS, paramKey, assertExists);
	}

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
	default Stream<Parameter<?>> streamOfParameters(String bagKey) {
		ParameterBag bag = getParameterBag(bagKey);
		if (bag == null)
			return Stream.empty();
		return bag.streamOfParameters();
	}

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
	default Stream<Parameter<?>> streamOfParametersByInterpretation(String bagKey, String interpretation) {
		ParameterBag bag = getParameterBag(bagKey);
		if (bag == null)
			return Stream.empty();
		return bag.streamOfParametersByInterpretation(interpretation);
	}

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
	default Stream<Parameter<?>> streamOfParametersByInterpretationAndUom(String bagKey, String interpretation,
			String uom) {
		ParameterBag bag = getParameterBag(bagKey);
		if (bag == null)
			return Stream.empty();
		return bag.streamOfParametersByInterpretationAndUom(interpretation, uom);
	}

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
	default List<Parameter<?>> getParametersByInterpretation(String bagKey, String interpretation) {
		return streamOfParametersByInterpretation(bagKey, interpretation).collect(toList());
	}

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
	default List<Parameter<?>> getParametersByInterpretationAndUom(String bagKey, String interpretation, String uom) {
		return streamOfParametersByInterpretationAndUom(bagKey, interpretation, uom).collect(toList());
	}

	/**
	 * Adds a new {@link Parameter} to the {@link ParameterBag} with the given key
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param parameter
	 * 		the {@link Parameter} to be added to the {@link ParameterBag}
	 *
	 * @throws StrolchException
	 * 		if the {@link ParameterBag} does not exist
	 */
	void addParameter(Parameter<?> parameter) throws StrolchException;

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
	 * Removes the {@link Parameter} with the given paramKey from the {@link ParameterBag} with the key
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be removed
	 *
	 * @return the removed {@link Parameter} or null if it did not exist
	 */
	<U, T extends Parameter<U>> T removeParameter(String paramKey);

	/**
	 * Removes the {@link Parameter} with the given paramKey from the {@link ParameterBag} with the key
	 * {@link StrolchModelConstants#BAG_RELATIONS}
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be removed
	 *
	 * @return the removed {@link Parameter} or null if it did not exist
	 */
	<U, T extends Parameter<U>> T removeRelation(String paramKey);

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
	default List<ParameterBag> getParameterBagsByType(String type) {
		return streamOfParameterBagsByType(type).collect(toList());
	}

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
	 * Returns true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} to be found
	 *
	 * @return true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 * bagKey. False is returned if the {@link ParameterBag} does not exist, or the {@link Parameter} does not exist on
	 * the {@link ParameterBag}
	 */
	default boolean hasParameter(String paramKey) {
		return hasParameter(BAG_PARAMETERS, paramKey);
	}

	/**
	 * Returns true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_RELATIONS}
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} to be found
	 *
	 * @return true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 * bagKey. False is returned if the {@link ParameterBag} does not exist, or the {@link Parameter} does not exist on
	 * the {@link ParameterBag}
	 */
	default boolean hasRelation(String paramKey) {
		return hasParameter(BAG_RELATIONS, paramKey);
	}

	/**
	 * Returns true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the id
	 * {@link StrolchModelConstants#BAG_RELATIONS} and the value of the parameter is also set, i.e. not empty
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} to be found
	 *
	 * @return true if the {@link Parameter} with the given paramKey exists on the {@link ParameterBag} with the given
	 * bagKey and the value of the parameter is also set, i.e. not empty. False is returned if the {@link ParameterBag}
	 * does not exist, the {@link Parameter} does not exist on the {@link ParameterBag}, or the value of the parameter
	 * is empty
	 */
	default boolean isRelationSet(String paramKey) {
		StringParameter relationP = getParameter(BAG_RELATIONS, paramKey);
		return relationP != null && relationP.isSet();
	}

	/**
	 * Returns true if the {@link Parameter} with the given paramKey does not exist on the {@link ParameterBag} with the
	 * id {@link StrolchModelConstants#BAG_RELATIONS} or if the value of the parameter is empty
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} to be found
	 *
	 * @return true if the {@link Parameter} with the given paramKey does not exist on the {@link ParameterBag} with the
	 * id {@link StrolchModelConstants#BAG_RELATIONS} or if the value of the parameter is empty
	 */
	default boolean isRelationEmpty(String paramKey) {
		StringParameter relationP = getParameter(BAG_RELATIONS, paramKey);
		return relationP == null || relationP.isEmpty();
	}

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

	/**
	 * Copies the value of the parameter with the given id from the given element and sets it on the parameter on this
	 * element. Copying is done from the #BAG_PARAMETERS parameter bag
	 *
	 * @param paramId
	 * 		the id of the parameter to fetch from the {@link ParameterBag} with the id #BAG_PARAMETERS
	 * @param otherElement
	 * 		the element from which to get the parameter
	 */
	default void copyParameterValue(String paramId, ParameterizedElement otherElement) {
		copyParameterValue(BAG_PARAMETERS, paramId, otherElement);
	}

	/**
	 * Copies the value of the parameter with the given id from the given element and sets it on the parameter on this
	 * element. Copying is done from the #BAG_RELATIONS parameter bag
	 *
	 * @param paramId
	 * 		the id of the parameter to fetch from the {@link ParameterBag} with the id #BAG_PARAMETERS
	 * @param otherElement
	 * 		the element from which to get the parameter
	 */
	default void copyRelationId(String paramId, ParameterizedElement otherElement) {
		copyParameterValue(BAG_RELATIONS, paramId, otherElement);
	}

	/**
	 * Copies the value of the parameter with the given id from the given element and sets it on the parameter on this
	 * element
	 *
	 * @param bagId
	 * 		the id of the bag from which to fetch the parameter
	 * @param paramId
	 * 		the id of the parameter to use
	 * @param otherElement
	 * 		the element from which to fetch the parameter
	 */
	default void copyParameterValue(String bagId, String paramId, ParameterizedElement otherElement) {
		Parameter<?> otherParam = otherElement.getParameter(paramId);
		getParameter(bagId, paramId, true).setValue(otherParam.getValue());
	}

	/**
	 * Copies the value of the parameter with the given id from the given element and sets it on the parameter on this
	 * element. Copying is done from the #BAG_PARAMETERS parameter bag
	 *
	 * @param paramId
	 * 		the id of the parameter to use
	 * @param otherElement
	 * 		the element from which to fetch the parameter
	 */
	default void copyParameterValue(String paramId, ParameterBagContainer otherElement) {
		copyParameterValue(BAG_PARAMETERS, paramId, otherElement);
	}

	/**
	 * Copies the value of the parameter with the given id from the given element and sets it on the parameter on this
	 * element. Copying is done from the #BAG_RELATIONS parameter bag
	 *
	 * @param paramId
	 * 		the id of the parameter to use
	 * @param otherElement
	 * 		the element from which to fetch the parameter
	 */
	default void copyRelationId(String paramId, ParameterBagContainer otherElement) {
		copyParameterValue(BAG_RELATIONS, paramId, otherElement);
	}

	/**
	 * Copies the value of the parameter with the given id from the given element and sets it on the parameter on this
	 * element
	 *
	 * @param bagId
	 * 		the id of the bag from which to fetch the parameter
	 * @param paramId
	 * 		the id of the parameter to use
	 * @param otherElement
	 * 		the element from which to fetch the parameter
	 */
	default void copyParameterValue(String bagId, String paramId, ParameterBagContainer otherElement) {
		Parameter<?> otherParam = otherElement.getParameter(bagId, paramId);
		getParameter(bagId, paramId, true).setValue(otherParam.getValue());
	}
}
