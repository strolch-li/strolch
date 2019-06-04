package li.strolch.report.policy;

import static java.util.Comparator.comparingInt;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.report.ReportConstants.*;
import static li.strolch.utils.helper.StringHelper.EMPTY;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.gson.JsonObject;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.*;
import li.strolch.model.parameter.AbstractParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.visitor.ElementDateVisitor;
import li.strolch.model.visitor.ElementStateVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.report.ReportConstants;
import li.strolch.report.ReportElement;
import li.strolch.runtime.StrolchConstants;
import li.strolch.utils.ObjectHelper;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfLists;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * A Generic Report defines a report as is described at <a href="https://strolch.li/documentation-reports.html">Strolch
 * Reports</a>
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class GenericReport extends ReportPolicy {

	private Resource reportRes;

	private ParameterBag columnsBag;
	private List<StringParameter> orderingParams;
	private Map<String, StringParameter> filterCriteriaParams;
	private boolean descending;
	private boolean allowMissingColumns;
	private List<String> columnIds;
	private StringParameter dateRangeSelP;

	private DateRange dateRange;
	private Map<ReportFilterPolicy, StringParameter> filtersByPolicy;
	private MapOfSets<String, String> filtersById;

	private long counter;

	protected JsonObject i18nData;

	public GenericReport(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	/**
	 * Retrieves the {@code Resource} with the given ID, and initializes this instance with the data specified on the
	 * report
	 *
	 * @param reportId
	 * 		the report to use
	 */
	public void initialize(String reportId) {

		// get the reportRes
		this.reportRes = tx().getResourceBy(TYPE_REPORT, reportId, true);

		StringParameter objectTypeP = this.reportRes.getParameter(BAG_PARAMETERS, PARAM_OBJECT_TYPE, true);
		String objectType = objectTypeP.getValue();

		this.columnsBag = this.reportRes.getParameterBag(BAG_COLUMNS, true);

		this.columnIds = this.columnsBag.getParameters().stream() //
				.sorted(comparingInt(Parameter::getIndex)) //
				.map(StrolchElement::getId) //
				.collect(Collectors.toList());

		if (this.reportRes.hasParameter(BAG_PARAMETERS, PARAM_DESCENDING))
			this.descending = this.reportRes.getParameter(BAG_PARAMETERS, PARAM_DESCENDING).getValue();
		if (this.reportRes.hasParameter(BAG_PARAMETERS, PARAM_ALLOW_MISSING_COLUMNS))
			this.allowMissingColumns = this.reportRes.getParameter(BAG_PARAMETERS, PARAM_ALLOW_MISSING_COLUMNS)
					.getValue();

		this.dateRangeSelP = this.reportRes.getParameter(BAG_PARAMETERS, PARAM_DATE_RANGE_SEL);

		// evaluate filter criteria params
		this.filterCriteriaParams = new HashMap<>();
		StringParameter objectTypeFilterCriteriaP = objectTypeP.getClone();
		objectTypeFilterCriteriaP.setId(objectType);
		this.filterCriteriaParams.put(objectType, objectTypeFilterCriteriaP);
		if (this.reportRes.hasParameterBag(BAG_JOINS)) {
			ParameterBag joinBag = this.reportRes.getParameterBag(BAG_JOINS);
			joinBag.getParameters()
					.forEach(parameter -> filterCriteriaParams.put(parameter.getId(), (StringParameter) parameter));
		}
		if (this.reportRes.hasParameterBag(BAG_ADDITIONAL_TYPE)) {
			ParameterBag additionalTypeBag = this.reportRes.getParameterBag(BAG_ADDITIONAL_TYPE);
			StringParameter additionalTypeP = additionalTypeBag.getParameter(PARAM_OBJECT_TYPE, true);
			this.filterCriteriaParams.put(additionalTypeP.getValue(), additionalTypeP);
		}
		if (this.reportRes.hasParameterBag(BAG_ADDITIONAL_JOINS)) {
			ParameterBag joinBag = this.reportRes.getParameterBag(BAG_ADDITIONAL_JOINS);
			joinBag.getParameters()
					.forEach(parameter -> filterCriteriaParams.put(parameter.getId(), (StringParameter) parameter));
		}

		// evaluate ordering params
		if (this.reportRes.hasParameterBag(BAG_ORDERING)) {
			ParameterBag orderingBag = this.reportRes.getParameterBag(BAG_ORDERING, true);
			if (orderingBag.hasParameters()) {
				this.orderingParams = orderingBag.getParameters().stream().map(e -> (StringParameter) e)
						.collect(Collectors.toList());
				this.orderingParams.sort(comparingInt(AbstractParameter::getIndex));
			}
		}

		// evaluate filters
		this.filtersByPolicy = new HashMap<>();
		List<ParameterBag> filterBags = this.reportRes.getParameterBagsByType(TYPE_FILTER);
		for (ParameterBag filterBag : filterBags) {

			// prepare filter function policy
			StringParameter functionP = filterBag.getParameter(PARAM_POLICY);
			PolicyHandler policyHandler = getContainer().getComponent(PolicyHandler.class);
			PolicyDef policyDef = PolicyDef.valueOf(functionP.getInterpretation(), functionP.getUom());
			ReportFilterPolicy filterFunction = policyHandler.getPolicy(policyDef, tx());
			filterFunction.init(functionP.getValue());

			StringParameter fieldRefP = filterBag.getParameter(PARAM_FIELD_REF);
			this.filtersByPolicy.put(filterFunction, fieldRefP);
		}
	}

	public boolean isDescending() {
		return this.descending;
	}

	public void setI18nData(JsonObject i18nData) {
		this.i18nData = i18nData;
	}

	public long getCounter() {
		return this.counter;
	}

	/**
	 * Returns true if the report has a date range selector specified
	 *
	 * @return true if the report has a date range selector specified
	 */
	public boolean hasDateRangeSelector() {
		return this.dateRangeSelP != null;
	}

	/**
	 * Sets the given date range
	 *
	 * @param dateRange
	 * 		the date range to set
	 *
	 * @return this for chaining
	 */
	public GenericReport dateRange(DateRange dateRange) {
		this.dateRange = dateRange;
		return this;
	}

	/**
	 * The keys for the header of this report, as is defined on the {@link ReportConstants#BAG_COLUMNS} parameter bag
	 *
	 * @return the keys for the header
	 */
	public List<String> getColumnKeys() {
		return this.columnIds;
	}

	/**
	 * Applies the given filter for the given element type
	 *
	 * @param type
	 * 		the type of element to filter
	 * @param ids
	 * 		the IDs of the elements to filter to
	 *
	 * @return this for chaining
	 */
	public GenericReport filter(String type, String... ids) {
		if (this.filtersById == null)
			this.filtersById = new MapOfSets<>();
		for (String id : ids) {
			this.filtersById.addElement(type, id);
		}
		return this;
	}

	/**
	 * Applies the given filter for the given element type
	 *
	 * @param type
	 * 		the type of element to filter
	 * @param ids
	 * 		the IDs of the elements to filter to
	 *
	 * @return this for chaining
	 */
	public GenericReport filter(String type, List<String> ids) {
		if (this.filtersById == null)
			this.filtersById = new MapOfSets<>();
		for (String id : ids) {
			this.filtersById.addElement(type, id);
		}
		return this;
	}

	/**
	 * Applies the given filter for the given element type
	 *
	 * @param type
	 * 		the type of element to filter
	 * @param ids
	 * 		the IDs of the elements to filter to
	 *
	 * @return this for chaining
	 */
	public GenericReport filter(String type, Set<String> ids) {
		if (this.filtersById == null)
			this.filtersById = new MapOfSets<>();
		for (String id : ids) {
			this.filtersById.addElement(type, id);
		}
		return this;
	}

	protected void incrementCounter() {
		this.counter++;
	}

	/**
	 * Builds the stream of rows on which further transformations can be performed. Each row is a {@link Map} for where
	 * the key is an element type, and the value is the associated element
	 *
	 * @return this for chaining
	 */
	public Stream<Map<String, StrolchRootElement>> buildStream() {

		Stream<Map<String, StrolchRootElement>> stream;

		// query the main objects and return a stream
		stream = queryRows() //
				// transform each element into a map of Type,Value pairs
				.map(this::evaluateRow);

		stream = handleAdditionalTypes(stream);

		if (hasFilter())
			stream = stream.filter(this::filter);

		stream = stream.peek(e -> this.incrementCounter());

		if (hasOrdering())
			stream = stream.sorted(this::sort);

		return stream;
	}

	/**
	 * Handles additional joining, so that we can join on arbitrary elements, matching on a {@link StringParameter}
	 *
	 * <p>
	 * the element we want to join is defined by the objectType parameter
	 * </p>
	 *
	 * <code>
	 * &lt;Parameter Id="objectType" Hidden="true" Name="Object Type" Type="String" Interpretation="Order-Ref"
	 * Uom="Order" Value="Order"/&gt;
	 * </code>
	 *
	 * <p>
	 * the joining is defined by two parameters, the joinParam defines the parameter for the additional join type to
	 * match
	 * </p>
	 *
	 * <code>
	 * &lt;Parameter Id="joinParam" Name="Join Param" Type="String" Value="Bags/relations/product"/&gt;
	 * </code>
	 *
	 * <p>
	 * and the joinWith parameter defines which already joined type and param to match on
	 * </p>
	 *
	 * <code>
	 * &lt;Parameter Id="joinWith" Name="Join With" Type="String" Interpretation="Resource-Ref" Uom="Slot"
	 * Value="Bags/relations/product"/&gt;
	 * </code>
	 *
	 * @param stream
	 * 		the current stream of rows
	 *
	 * @return the new stream of rows, which iterates over the additionally joined elements, thus creating a cartesian
	 * product stream
	 */
	protected Stream<Map<String, StrolchRootElement>> handleAdditionalTypes(
			Stream<Map<String, StrolchRootElement>> stream) {

		// see if we need to do additional type joining
		ParameterBag additionalTypeBag = this.reportRes.getParameterBag(BAG_ADDITIONAL_TYPE);
		if (additionalTypeBag == null)
			return stream;

		StringParameter objectTypeP = additionalTypeBag.getParameter(PARAM_OBJECT_TYPE, true);

		StringParameter joinParamP = additionalTypeBag.getParameter(PARAM_JOIN_PARAM, true);
		String[] locatorParts = joinParamP.getValue().split(Locator.PATH_SEPARATOR);
		if (locatorParts.length != 3)
			throw new IllegalStateException(
					"Parameter reference (" + joinParamP.getValue() + ") is invalid as it does not have 3 parts for "
							+ joinParamP.getLocator());
		String bagKey = locatorParts[1];
		String paramKey = locatorParts[2];

		MapOfLists<String, StrolchRootElement> joinElements = getStreamFor(objectTypeP)
				.collect(MapOfLists::new, (mapOfLists, e) -> {
					StringParameter joinP = e.getParameter(bagKey, paramKey, true);
					mapOfLists.addElement(joinP.getValue(), e);
				}, MapOfLists::addAll);

		StringParameter joinWithP = additionalTypeBag.getParameter(PARAM_JOIN_WITH, true);
		return stream.flatMap(row -> {

			StrolchRootElement joinElement = row.get(joinWithP.getUom());
			if (joinElement == null)
				throw new IllegalStateException(
						"Additional join type " + joinWithP.getUom() + " is not available on row for " + joinWithP
								.getLocator());

			Optional<Parameter<?>> refP = lookupParameter(joinWithP, joinElement);
			if (!refP.isPresent()) {
				throw new IllegalStateException(
						"Parameter reference (" + joinWithP.getValue() + ") for " + joinWithP.getLocator()
								+ " not found on " + joinElement.getLocator());
			}

			StringParameter joinP = (StringParameter) refP.get();
			if (joinP.isEmpty())
				return Stream.of(row);
			List<StrolchRootElement> elements = joinElements.getList(joinP.getValue());
			if (elements == null)
				return Stream.of(row);

			return elements.stream().map(additionalElement -> {
				Map<String, StrolchRootElement> additionalRow = new HashMap<>(row);
				additionalRow.put(additionalElement.getType(), additionalElement);
				handleJoins(additionalRow, BAG_ADDITIONAL_JOINS);
				return additionalRow;
			});
		});
	}

	/**
	 * Performs the report, returning a stream of {@link ReportElement}
	 *
	 * @return this for chaining
	 */
	public Stream<ReportElement> doReport() {

		return buildStream().map(e -> new ReportElement(this.columnIds, columnId -> {
			StringParameter columnDefP = this.columnsBag.getParameter(columnId, true);
			Object value = evaluateColumnValue(columnDefP, e);
			if (value instanceof Date)
				return ISO8601FormatFactory.getInstance().formatDate((Date) value);
			if (value instanceof Parameter)
				return ((Parameter) value).getValueAsString();
			return value.toString();
		}));
	}

	/**
	 * <p>Check to see if the given element is to be filtered, i.e. is to be kept in the stream</p>
	 *
	 * <p>This implementation checks if a join parameters is set as hidden, including if the parameter {@link
	 * ReportConstants#PARAM_OBJECT_TYPE} is defined as hidden</p>
	 *
	 * <p>This method can be overridden for further filtering</p>
	 *
	 * @param element
	 * 		the element to filter
	 *
	 * @return true if the element is to be kept, false if not
	 */
	protected boolean filterCriteria(StrolchRootElement element) {
		return !this.filterCriteriaParams.get(element.getType()).isHidden();
	}

	/**
	 * Generates the filter criteria for this report, i.e. it returns a {@link MapOfSets} which defines the type of
	 * elements on which a filter can be set and the {@link Set} of IDs which can be used for filtering.
	 *
	 * @return the filter criteria as a map of sets
	 */
	public MapOfSets<String, StrolchRootElement> generateFilterCriteria() {
		return buildStream() //

				.flatMap(e -> e.values().stream().filter(this::filterCriteria)) //

				.sorted((o1, o2) -> {
					String type1 = o1.getType();
					String type2 = o2.getType();
					if (type1.equals(type2))
						return 0;
					StringParameter p1 = this.filterCriteriaParams.get(type1);
					StringParameter p2 = this.filterCriteriaParams.get(type2);
					if (p1 == null && p2 == null)
						return type1.compareTo(type2);
					if (p1 == null)
						return 1;
					if (p2 == null)
						return -1;

					return Integer.compare(p1.getIndex(), p2.getIndex());
				}) //

				.collect( //
						Collector.of( //
								(Supplier<MapOfSets<String, StrolchRootElement>>) () -> new MapOfSets<>(
										new LinkedHashMap<>()), //
								(m, e) -> m.addElement(e.getType(), e), //
								MapOfSets::addAll, //
								m -> m));
	}

	/**
	 * Returns true if an ordering is defined by means of {@link ReportConstants#BAG_ORDERING}
	 *
	 * @return true if an ordering is defined
	 */
	protected boolean hasOrdering() {
		return this.orderingParams != null && !this.orderingParams.isEmpty();
	}

	/**
	 * Implements a sorting of the given two rows. This implementation using the ordering as is defined in {@link
	 * ReportConstants#BAG_ORDERING}
	 *
	 * @param row1
	 * 		the left side
	 * @param row2
	 * 		the right side
	 *
	 * @return the value {@code -1}, {@code 0} or {@code 1}, depending on the defined ordering
	 */
	protected int sort(Map<String, StrolchRootElement> row1, Map<String, StrolchRootElement> row2) {

		for (StringParameter fieldRefP : this.orderingParams) {

			String type = fieldRefP.getUom();

			StrolchRootElement column1 = row1.get(type);
			StrolchRootElement column2 = row2.get(type);
			if (column1 == null && column2 == null)
				continue;
			if (column1 == null)
				return -1;
			if (column2 == null)
				return 1;

			int sortVal;
			if (fieldRefP.getValue().startsWith("$")) {
				Object columnValue1 = evaluateColumnValue(fieldRefP, row1);
				Object columnValue2 = evaluateColumnValue(fieldRefP, row2);

				if (this.descending) {
					sortVal = ObjectHelper.compare(columnValue2, columnValue1, true);
				} else {
					sortVal = ObjectHelper.compare(columnValue1, columnValue2, true);
				}

			} else {
				Optional<Parameter<?>> param1 = lookupParameter(fieldRefP, column1);
				Optional<Parameter<?>> param2 = lookupParameter(fieldRefP, column2);

				if (!param1.isPresent() && !param2.isPresent())
					continue;

				if (param1.isPresent() && !param2.isPresent())
					return 1;
				else if (!param1.isPresent())
					return -1;

				if (this.descending)
					sortVal = param2.get().compareTo(param1.get());
				else
					sortVal = param1.get().compareTo(param2.get());
			}

			if (sortVal != 0)
				return sortVal;
		}

		return 0;
	}

	/**
	 * Returns true if a filter is defined, i.e. {@link ParameterBag ParameterBags} of type {@link
	 * ReportConstants#TYPE_FILTER}, a date range
	 *
	 * @return true if a filter is defined
	 */
	protected boolean hasFilter() {
		return !this.filtersByPolicy.isEmpty() || this.dateRange != null || (this.filtersById != null
				&& !this.filtersById.isEmpty());
	}

	/**
	 * Returns true if the element is filtered, i.e. is to be kep, false if it should not be kept in the stream
	 *
	 * @param row
	 * 		the row to check if it is filtered
	 *
	 * @return if the element is filtered
	 */
	protected boolean filter(Map<String, StrolchRootElement> row) {

		// do filtering by policies
		for (ReportFilterPolicy filterPolicy : this.filtersByPolicy.keySet()) {
			StringParameter fieldRefP = this.filtersByPolicy.get(filterPolicy);

			String type = fieldRefP.getUom();

			StrolchRootElement column = row.get(type);

			// if column is null, then don't include in result
			if (column == null)
				return false;

			if (fieldRefP.getValue().startsWith("$")) {
				Object value = evaluateColumnValue(fieldRefP, row);
				if (!filterPolicy.filter(value))
					return false;
			} else {
				Optional<Parameter<?>> param = lookupParameter(fieldRefP, column);
				if (param.isPresent() && !filterPolicy.filter(param.get()))
					return false;
			}
		}

		// do a date range selection, if required
		if (this.dateRange != null) {
			if (this.dateRangeSelP == null)
				throw new IllegalStateException(
						"DateRange defined, but reportRes does not defined a date range selector!");

			String type = this.dateRangeSelP.getUom();
			StrolchRootElement element = row.get(type);
			if (element == null)
				return false;

			String dateRangeSel = this.dateRangeSelP.getValue();

			Date date;
			if (dateRangeSel.equals(COL_DATE)) {
				date = element.accept(new ElementDateVisitor());
			} else {
				Optional<Parameter<?>> param = lookupParameter(this.dateRangeSelP, element);
				if (!param.isPresent() || StrolchValueType.parse(param.get().getType()) != StrolchValueType.DATE)
					throw new IllegalStateException(
							"Date Range selector is invalid, as referenced parameter is not a Date but " + (param
									.isPresent() ? param.get().getType() : "null"));

				date = ((DateParameter) param.get()).getValue();
			}

			if (!this.dateRange.contains(date))
				return false;
		}

		// then we do a filter by criteria
		if (this.filtersById != null && !this.filtersById.isEmpty()) {
			for (String type : this.filtersById.keySet()) {
				StrolchRootElement element = row.get(type);
				if (element == null)
					return false;

				if (!this.filtersById.getSet(type).contains(element.getId()))
					return false;
			}
		}

		// otherwise we want to keep this row
		return true;
	}

	/**
	 * Evaluates the column value from the given column definition and row
	 *
	 * @param columnDefP
	 * 		the column definition
	 * @param row
	 * 		the row
	 *
	 * @return the column value
	 */
	protected Object evaluateColumnValue(StringParameter columnDefP, Map<String, StrolchRootElement> row) {

		String columnDef = columnDefP.getValue();
		String refType = columnDefP.getUom();

		// get the referenced object
		StrolchRootElement column = row.get(refType);

		Object columnValue;

		if (column == null) {
			columnValue = EMPTY;
		} else if (columnDef.equals(COL_OBJECT)) {
			columnValue = column;
		} else if (columnDef.equals(COL_ID)) {
			columnValue = column.getId();
		} else if (columnDef.equals(COL_NAME)) {
			columnValue = column.getName();
		} else if (columnDef.equals(COL_TYPE)) {
			columnValue = column.getType();
		} else if (columnDef.equals(COL_STATE)) {
			columnValue = column.accept(new ElementStateVisitor());
		} else if (columnDef.equals(COL_DATE)) {
			columnValue = column.accept(new ElementDateVisitor());
		} else if (columnDef.startsWith(COL_SEARCH)) {
			Parameter<?> parameter = findParameter(columnDefP, column);
			if (parameter == null)
				columnValue = EMPTY;
			else
				columnValue = parameter;
		} else {
			columnValue = lookupParameter(columnDefP, column).orElseGet(StringParameter::new);
		}

		return columnValue;
	}

	/**
	 * Finds a parameter given the column definition
	 *
	 * @param columnDefP
	 * 		the column definition
	 * @param column
	 * 		the element from which the parameter is to be retrieved
	 *
	 * @return the parameter, or null if it does not exist
	 */
	protected Parameter<Object> findParameter(StringParameter columnDefP, StrolchRootElement column) {

		String columnDef = columnDefP.getValue();

		String[] searchParts = columnDef.split(SEARCH_SEPARATOR);
		if (searchParts.length != 3)
			throw new IllegalStateException(
					"Parameter search reference (" + columnDef + ") is invalid as it does not have 3 parts for "
							+ columnDefP.getLocator());

		String parentParamId = searchParts[1];
		String paramRef = searchParts[2];

		String[] locatorParts = paramRef.split(Locator.PATH_SEPARATOR);
		if (locatorParts.length != 3)
			throw new IllegalStateException(
					"Parameter search reference (" + paramRef + ") is invalid as it does not have 3 parts for "
							+ columnDefP.getLocator());

		String bagKey = locatorParts[1];
		String paramKey = locatorParts[2];

		Optional<Parameter<Object>> parameter = tx().findParameterOnHierarchy(column, parentParamId, bagKey, paramKey);
		return parameter.orElse(null);
	}

	/**
	 * Retrieves the given parameter with the given parameter reference from the given column
	 *
	 * @param paramRefP
	 * 		the parameter reference
	 * @param element
	 * 		the element
	 *
	 * @return the {@link Optional} with the parameter
	 */
	protected Optional<Parameter<?>> lookupParameter(StringParameter paramRefP, StrolchRootElement element) {
		String paramRef = paramRefP.getValue();

		String[] locatorParts = paramRef.split(Locator.PATH_SEPARATOR);
		if (locatorParts.length != 3)
			throw new IllegalStateException(
					"Parameter reference (" + paramRef + ") is invalid as it does not have 3 parts for " + paramRefP
							.getLocator());

		String bagKey = locatorParts[1];
		String paramKey = locatorParts[2];

		Parameter<?> param = element.getParameter(bagKey, paramKey);
		if (!allowMissingColumns && param == null)
			throw new IllegalStateException(
					"Parameter reference (" + paramRef + ") for " + paramRefP.getLocator() + " not found on " + element
							.getLocator());

		return Optional.ofNullable(param);
	}

	/**
	 * Returns a stream of {@link StrolchRootElement} which denote the rows of the report. This implementation uses
	 * {@link ReportConstants#PARAM_OBJECT_TYPE} to stream the initial rows
	 *
	 * @return the stream of {@link StrolchRootElement StrolchRootElement}
	 */
	protected Stream<? extends StrolchRootElement> queryRows() {
		StringParameter objectTypeP = this.reportRes.getParameter(BAG_PARAMETERS, PARAM_OBJECT_TYPE);
		return getStreamFor(objectTypeP);
	}

	private Stream<? extends StrolchRootElement> getStreamFor(StringParameter objectTypeP) {
		switch (objectTypeP.getInterpretation()) {
		case StrolchConstants.INTERPRETATION_RESOURCE_REF:
			return tx().streamResources(objectTypeP.getUom());
		case StrolchConstants.INTERPRETATION_ORDER_REF:
			return tx().streamOrders(objectTypeP.getUom());
		case StrolchConstants.INTERPRETATION_ACTIVITY_REF:
			return tx().streamActivities(objectTypeP.getUom());
		default:
			throw new IllegalArgumentException("Unhandled element type " + objectTypeP.getInterpretation());
		}
	}

	/**
	 * Evaluates the row for the given element. The resulting {@link Map} contains the joins on all elements and the
	 * keys are the type of elements and values are the actual elements
	 *
	 * @param element
	 * 		the element from which the row is evaluated
	 *
	 * @return the {@link Map} of elements denoting the row for the given element
	 */
	protected Map<String, StrolchRootElement> evaluateRow(StrolchRootElement element) {

		// interpretation -> Resource-Ref, etc.
		// uom -> object type
		// value -> element type where relation is defined for this join

		// create the refs element
		Map<String, StrolchRootElement> refs = new HashMap<>();
		// and add the starting point
		refs.put(element.getType(), element);

		handleJoins(refs, BAG_JOINS);
		return refs;
	}

	private void handleJoins(Map<String, StrolchRootElement> refs, String joinBagId) {
		ParameterBag joinBag = this.reportRes.getParameterBag(joinBagId);
		if (joinBag != null && joinBag.hasParameters()) {
			for (String paramId : joinBag.getParameterKeySet()) {
				StringParameter joinP = joinBag.getParameter(paramId);
				addColumnJoin(refs, joinBag, joinP, true);
			}
		}
	}

	/**
	 * Finds the join with the given elements
	 *
	 * @param refs
	 * 		the current row, with any already retrieved joins
	 * @param joinBag
	 * 		the {@link ReportConstants#BAG_JOINS} {@link ParameterBag}
	 * @param joinP
	 * 		the join definition
	 * @param optional
	 * 		a boolean defining if the join my be missing
	 *
	 * @return the joined element, or null if it does not exist and {@code optional} is false
	 */
	protected StrolchRootElement addColumnJoin(Map<String, StrolchRootElement> refs, ParameterBag joinBag,
			StringParameter joinP, boolean optional) {

		String interpretation = joinP.getInterpretation();
		String elementType = interpretation.substring(0, interpretation.indexOf(SUFFIX_REF));
		String joinType = joinP.getUom();
		String dependencyType = joinP.getValue();

		// get dependency
		StrolchRootElement dependency;
		if (refs.containsKey(dependencyType)) {
			dependency = refs.get(dependencyType);
		} else {
			// recursively find the dependency
			StringParameter dependencyP = joinBag.getParameter(dependencyType);
			dependency = addColumnJoin(refs, joinBag, dependencyP, false);
			if (dependency == null)
				return null;
		}

		ParameterBag relationsBag = dependency.getParameterBag(BAG_RELATIONS);
		if (relationsBag == null)
			throw new IllegalStateException(
					"Invalid join definition value: " + joinP.getValue() + " on: " + joinP.getLocator() + " as "
							+ dependency.getLocator() + " has no ParameterBag " + BAG_RELATIONS);

		List<Parameter<?>> relationParams = relationsBag.getParametersByInterpretationAndUom(interpretation, joinType)
				.stream().filter(p -> p.getValueType() == StrolchValueType.STRING).collect(Collectors.toList());

		if (relationParams.isEmpty()) {
			throw new IllegalStateException(
					"Found no relation parameters with UOM " + joinType + " of type " + StrolchValueType.STRING
							.getType() + " on dependency " + dependency.getLocator());
		}
		if (relationParams.size() > 1) {
			throw new IllegalStateException(
					"Found multiple possible relation parameters for UOM " + joinType + " on dependency " + dependency
							.getLocator());
		}

		Parameter<?> relationParam = relationParams.get(0);
		StringParameter relationP = (StringParameter) relationParam;
		if (relationP.getValue().isEmpty() && optional) {
			return null;
		}

		Locator locator = Locator.valueOf(elementType, joinType, relationP.getValue());
		StrolchRootElement joinElem = tx().findElement(locator, true);
		if (joinElem == null)
			return null;

		refs.put(joinType, joinElem);
		return joinElem;
	}
}
