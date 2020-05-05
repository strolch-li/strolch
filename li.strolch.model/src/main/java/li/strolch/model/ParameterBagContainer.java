package li.strolch.model;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import li.strolch.exception.StrolchException;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.parameter.*;
import li.strolch.utils.time.PeriodDuration;

/**
 * A {@link ParameterBagContainer} has a map of {@link ParameterBag ParameterBags} where the key is the id of the
 * parameter bag. This allows to group {@link Parameter Parameters} in strolch objects
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ParameterBagContainer extends StrolchElement {

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
	String getString(String paramKey) throws StrolchModelException;

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
	String getString(String bagKey, String paramKey) throws StrolchModelException;

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
	boolean getBoolean(String paramKey) throws StrolchModelException;

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
	boolean getBoolean(String bagKey, String paramKey) throws StrolchModelException;

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
	int getInteger(String paramKey) throws StrolchModelException;

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
	int getInteger(String bagKey, String paramKey) throws StrolchModelException;

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
	double getDouble(String paramKey) throws StrolchModelException;

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
	double getDouble(String bagKey, String paramKey) throws StrolchModelException;

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
	long getLong(String paramKey) throws StrolchModelException;

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
	long getLong(String bagKey, String paramKey) throws StrolchModelException;

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
	ZonedDateTime getDate(String paramKey) throws StrolchModelException;

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
	ZonedDateTime getDate(String bagKey, String paramKey) throws StrolchModelException;

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
	LocalDateTime getLocalDate(String paramKey) throws StrolchModelException;

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
	LocalDateTime getLocalDate(String bagKey, String paramKey) throws StrolchModelException;

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
	String getText(String paramKey) throws StrolchModelException;

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
	String getText(String bagKey, String paramKey) throws StrolchModelException;

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
	PeriodDuration getDuration(String paramKey) throws StrolchModelException;

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
	PeriodDuration getDuration(String bagKey, String paramKey) throws StrolchModelException;

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
	List<String> getStringList(String paramKey) throws StrolchModelException;

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
	List<String> getStringList(String bagKey, String paramKey) throws StrolchModelException;

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
	List<Integer> getIntegerList(String paramKey) throws StrolchModelException;

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
	List<Integer> getIntegerList(String bagKey, String paramKey) throws StrolchModelException;

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
	List<Double> getDoubleList(String paramKey) throws StrolchModelException;

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
	List<Double> getDoubleList(String bagKey, String paramKey) throws StrolchModelException;

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
	List<Long> getLongList(String paramKey) throws StrolchModelException;

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
	List<Long> getLongList(String bagKey, String paramKey) throws StrolchModelException;

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
	void setString(String paramKey, String value) throws StrolchModelException;

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
	void setString(String bagKey, String paramKey, String value) throws StrolchModelException;

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
	void setBoolean(String paramKey, boolean value) throws StrolchModelException;

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
	void setBoolean(String bagKey, String paramKey, boolean value) throws StrolchModelException;

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
	void setInteger(String paramKey, int value) throws StrolchModelException;

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
	void setInteger(String bagKey, String paramKey, int value) throws StrolchModelException;

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
	void setDouble(String paramKey, double value) throws StrolchModelException;

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
	void setDouble(String bagKey, String paramKey, double value) throws StrolchModelException;

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
	void setLong(String paramKey, long value) throws StrolchModelException;

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
	void setLong(String bagKey, String paramKey, long value) throws StrolchModelException;

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
	void setDate(String paramKey, ZonedDateTime value) throws StrolchModelException;

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
	void setDate(String bagKey, String paramKey, ZonedDateTime value) throws StrolchModelException;

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
	void setDate(String paramKey, LocalDateTime value) throws StrolchModelException;

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
	void setDate(String bagKey, String paramKey, LocalDateTime value) throws StrolchModelException;

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
	void setText(String paramKey, String value) throws StrolchModelException;

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
	void setText(String bagKey, String paramKey, String value) throws StrolchModelException;

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
	void setDuration(String paramKey, PeriodDuration value) throws StrolchModelException;

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
	void setDuration(String bagKey, String paramKey, PeriodDuration value) throws StrolchModelException;

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
	void setStringList(String paramKey, List<String> value) throws StrolchModelException;

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
	void setStringList(String bagKey, String paramKey, List<String> value) throws StrolchModelException;

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
	void setIntegerList(String paramKey, List<Integer> value) throws StrolchModelException;

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
	void setIntegerList(String bagKey, String paramKey, List<Integer> value) throws StrolchModelException;

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
	void setDoubleList(String paramKey, List<Double> value) throws StrolchModelException;

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
	void setDoubleList(String bagKey, String paramKey, List<Double> value) throws StrolchModelException;

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
	void setLongList(String paramKey, List<Long> value) throws StrolchModelException;

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
	void setLongList(String bagKey, String paramKey, List<Long> value) throws StrolchModelException;

	/**
	 * Returns the {@link StringParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	StringParameter getStringP(String paramKey) throws StrolchModelException;

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
	StringParameter getStringP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link BooleanParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	BooleanParameter getBooleanP(String paramKey) throws StrolchModelException;

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
	BooleanParameter getBooleanP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link IntegerParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	IntegerParameter getIntegerP(String paramKey) throws StrolchModelException;

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
	IntegerParameter getIntegerP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link FloatParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	FloatParameter getDoubleP(String paramKey) throws StrolchModelException;

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
	FloatParameter getDoubleP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link LongParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	LongParameter getLongP(String paramKey) throws StrolchModelException;

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
	LongParameter getLongP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link DateParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	DateParameter getDateP(String paramKey) throws StrolchModelException;

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
	DateParameter getDateP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link TextParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	TextParameter getTextP(String paramKey) throws StrolchModelException;

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
	TextParameter getTextP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link DurationParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	DurationParameter getDurationP(String paramKey) throws StrolchModelException;

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
	DurationParameter getDurationP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link StringListParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	StringListParameter getStringListP(String paramKey) throws StrolchModelException;

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
	StringListParameter getStringListP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link IntegerListParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	IntegerListParameter getIntegerListP(String paramKey) throws StrolchModelException;

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
	IntegerListParameter getIntegerListP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link FloatListParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	FloatListParameter getDoubleListP(String paramKey) throws StrolchModelException;

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
	FloatListParameter getDoubleListP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link LongListParameter} with the given paramKey from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the parameter
	 *
	 * @return the parameter with the given paramKey
	 *
	 * @throws StrolchModelException
	 * 		if the parameter does not exist
	 */
	LongListParameter getLongListP(String paramKey) throws StrolchModelException;

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
	LongListParameter getLongListP(String bagKey, String paramKey) throws StrolchModelException;

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the ID {@link *
	 * StrolchModelConstants#BAG_PARAMETERS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	<U, T extends Parameter<U>> T getParameter(String paramKey);

	/**
	 * Returns the {@link Parameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_PARAMETERS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
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
	 * Returns the {@link StringParameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	StringParameter getRelationParam(String paramKey);

	/**
	 * Returns the {@link StringParameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	StringParameter getRelationParam(String paramKey, boolean assertExists);

	/**
	 * Returns the {@link StringListParameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	StringListParameter getRelationsParam(String paramKey);

	/**
	 * Returns the {@link StringListParameter} with the given key from the {@link ParameterBag} with the ID {@link
	 * StrolchModelConstants#BAG_RELATIONS}, or null if the {@link Parameter} or the {@link ParameterBag} does not
	 * exist
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be returned
	 * @param assertExists
	 * 		if set to true, and the parameter does not exist, a {@link StrolchModelException} is thrown
	 *
	 * @return the found {@link Parameter} or null if it was not found
	 */
	StringListParameter getRelationsParam(String paramKey, boolean assertExists);

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
	 * Adds a new {@link Parameter} to the {@link ParameterBag} with the given key {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
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
	 * Removes the {@link Parameter} with the given paramKey from the {@link ParameterBag} with the key {@link
	 * StrolchModelConstants#BAG_PARAMETERS}
	 *
	 * @param paramKey
	 * 		the key of the {@link Parameter} which is to be removed
	 *
	 * @return the removed {@link Parameter} or null if it did not exist
	 */
	<U, T extends Parameter<U>> T removeParameter(String paramKey);

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
	boolean hasParameter(String paramKey);

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
	boolean hasRelation(String paramKey);

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
