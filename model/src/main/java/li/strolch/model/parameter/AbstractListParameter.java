package li.strolch.model.parameter;

import static java.util.stream.Collectors.joining;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Stream;

import li.strolch.exception.StrolchException;
import li.strolch.utils.helper.StringHelper;

public abstract class AbstractListParameter<E> extends AbstractParameter<List<E>> implements ListParameter<E> {

	protected List<E> value;

	/**
	 * Empty constructor
	 */
	AbstractListParameter() {
		//
	}

	/**
	 * Default constructor
	 *
	 * @param id
	 * 		the id
	 * @param name
	 * 		the name
	 */
	AbstractListParameter(String id, String name) {
		super(id, name);
	}

	protected abstract void validateElement(E value);

	protected abstract String elementToString(E element);

	protected abstract List<E> parseString(String valueAsString);

	/**
	 * Validates that the value is legal. This is the case when it is not null in this implementation
	 *
	 * @param values
	 * 		the values to check for this parameter instance
	 *
	 * @throws StrolchException
	 * 		if the value is null
	 */
	protected void validateValue(List<E> values) throws StrolchException {
		if (values == null) {
			String msg = "Can not set null value on Parameter {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getLocator());
			throw new StrolchException(msg);
		}

		values.forEach(this::validateElement);
	}

	/**
	 * Validates that the value is legal. This is the case when it is not null in this implementation
	 *
	 * @param values
	 * 		the values to check for this parameter instance
	 *
	 * @throws StrolchException
	 * 		if the value is null
	 */
	protected void validateValue(Collection<E> values) throws StrolchException {
		if (values == null) {
			String msg = "Can not set null value on Parameter {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getLocator());
			throw new StrolchException(msg);
		}

		values.forEach(this::validateElement);
	}

	@Override
	public String getValueAsString() {
		if (this.value.isEmpty())
			return StringHelper.EMPTY;
		return this.value.stream().map(this::elementToString).collect(joining(", "));
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<E> getValue() {
		return new ArrayList<>(this.value);
	}

	@Override
	public E getValue(int index) {
		if (this.value.isEmpty() || this.value.size() < index)
			throw new IllegalStateException("No value at index " + index + " for " + getLocator());
		return this.value.get(index);
	}

	@Override
	public Stream<E> streamValues() {
		return this.value.stream();
	}

	@Override
	public void setValue(List<E> values) {
		assertNotReadonly();
		validateValue(values);
		this.value = new ArrayList<>(values);
	}

	@Override
	public void setValue(Collection<E> values) {
		assertNotReadonly();
		validateValue(values);
		this.value = new ArrayList<>(values);
	}

	@Override
	public void setValueFrom(Parameter<List<E>> parameter) {
		assertNotReadonly();
		this.value = new ArrayList<>(parameter.getValue());
	}

	@Override
	public void setValueFromString(String valueAsString) {
		setValue(parseString(valueAsString));
	}

	@Override
	public void addValue(E value) {
		assertNotReadonly();
		validateElement(value);
		this.value.add(value);
	}

	@Override
	public void addAllValues(List<E> values) {
		assertNotReadonly();
		for (E value : values) {
			validateElement(value);
			this.value.add(value);
		}
	}

	@Override
	public void addAllValuesIfNotContains(List<E> values) {
		assertNotReadonly();
		for (E value : values) {
			validateElement(value);
			if (!this.value.contains(value))
				this.value.add(value);
		}
	}

	@Override
	public boolean addValueIfNotContains(E value) {
		assertNotReadonly();
		validateElement(value);

		if (this.value.contains(value))
			return false;

		this.value.add(value);
		return true;
	}

	@Override
	public boolean removeValue(E value) {
		assertNotReadonly();
		return this.value.remove(value);
	}

	@Override
	public void clear() {
		assertNotReadonly();
		if (this.value == null)
			this.value = new ArrayList<>();
		else
			this.value.clear();
	}

	@Override
	public boolean isEmpty() {
		return this.value.isEmpty();
	}

	@Override
	public boolean isEqualTo(Parameter<List<E>> otherValue) {
		AbstractListParameter<E> other = (AbstractListParameter<E>) otherValue;
		return this.value.equals(other.value);
	}

	@Override
	public boolean isEqualTo(List<E> otherValue) {
		return this.value.equals(otherValue);
	}

	@Override
	public int size() {
		return this.value.size();
	}

	@Override
	public boolean contains(E value) {
		return this.value.contains(value);
	}

	@Override
	public boolean containsAll(List<E> values) {
		return this.value.containsAll(values);
	}
}
