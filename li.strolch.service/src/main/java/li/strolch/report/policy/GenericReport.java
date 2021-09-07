package li.strolch.report.policy;

import static java.util.Comparator.comparing;
import static java.util.Comparator.comparingInt;
import static java.util.stream.Collectors.toList;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.report.ReportConstants.*;
import static li.strolch.utils.helper.StringHelper.EMPTY;

import java.time.ZonedDateTime;
import java.util.*;
import java.util.stream.Stream;

import com.google.gson.JsonObject;
import li.strolch.model.*;
import li.strolch.model.parameter.AbstractParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.visitor.ElementStateVisitor;
import li.strolch.model.visitor.ElementZdtDateVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.report.ReportConstants;
import li.strolch.report.ReportElement;
import li.strolch.utils.ObjectHelper;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfLists;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.collections.TypedTuple;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601;

/**
 * A Generic Report defines a report as is described at <a href="https://strolch.li/documentation-reports.html">Strolch
 * Reports</a>
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class GenericReport extends ReportPolicy {

	protected Resource reportRes;

	protected ParameterBag columnsBag;
	protected List<StringParameter> orderingParams;
	protected Map<String, StringParameter> filterCriteriaParams;
	protected boolean parallel;
	protected boolean descending;
	protected boolean allowMissingColumns;
	protected boolean filterMissingValuesAsTrue;
	protected List<String> columnIds;
	protected StringParameter dateRangeSelP;

	protected DateRange dateRange;
	protected Map<ReportFilterPolicy, TypedTuple<StringParameter, StringParameter>> filtersByPolicy;
	protected MapOfSets<String, String> filtersById;

	protected long counter;
	protected boolean withPage;
	protected int offset = -1;
	protected int limit = -1;

	protected JsonObject i18nData;

	public GenericReport(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * Retrieves the {@code Resource} with the given ID, and initializes this instance with the data specified on the
	 * report
	 *
	 * @param reportId
	 * 		the report to use
	 */
	@Override
	public void initialize(String reportId) {

		// get the reportRes
		this.reportRes = tx().getResourceBy(TYPE_REPORT, reportId, true);

		StringParameter objectTypeP = this.reportRes.getStringP(PARAM_OBJECT_TYPE);
		String objectType = objectTypeP.getValue();

		this.columnsBag = this.reportRes.getParameterBag(BAG_COLUMNS, true);

		this.columnIds = this.columnsBag.getParameters().stream() //
				.sorted(comparingInt(Parameter::getIndex)) //
				.map(StrolchElement::getId) //
				.collect(toList());

		this.parallel = this.reportRes.getBoolean(PARAM_PARALLEL);
		this.descending = this.reportRes.getBoolean(PARAM_DESCENDING);
		this.allowMissingColumns = this.reportRes.getBoolean(PARAM_ALLOW_MISSING_COLUMNS);
		this.filterMissingValuesAsTrue = this.reportRes.getBoolean(PARAM_FILTER_MISSING_VALUES_AS_TRUE);
		this.dateRangeSelP = this.reportRes.getParameter(BAG_PARAMETERS, PARAM_DATE_RANGE_SEL);

		// evaluate filter criteria params
		this.filterCriteriaParams = new HashMap<>();
		StringParameter objectTypeFilterCriteriaP = objectTypeP.getClone();
		objectTypeFilterCriteriaP.setId(objectType);
		if (objectTypeFilterCriteriaP.getUom().equals(UOM_NONE))
			throw new IllegalStateException(
					"Join UOM " + objectTypeFilterCriteriaP.getUom() + " invalid: " + objectTypeFilterCriteriaP.getId()
							+ " for " + objectTypeFilterCriteriaP.getLocator());
		this.filterCriteriaParams.put(objectType, objectTypeFilterCriteriaP);
		if (this.reportRes.hasParameterBag(BAG_JOINS)) {
			ParameterBag joinBag = this.reportRes.getParameterBag(BAG_JOINS);
			joinBag.getParameters().forEach(parameter -> {
				StringParameter joinP = (StringParameter) parameter;
				if (joinP.getUom().equals(UOM_NONE))
					throw new IllegalStateException(
							"Join UOM " + joinP.getUom() + " invalid: " + joinP.getId() + " for " + joinP.getLocator());
				this.filterCriteriaParams.put(parameter.getId(), joinP);
			});
		}
		if (this.reportRes.hasParameterBag(BAG_ADDITIONAL_TYPE)) {
			ParameterBag additionalTypeBag = this.reportRes.getParameterBag(BAG_ADDITIONAL_TYPE);
			StringParameter additionalTypeP = additionalTypeBag.getParameter(PARAM_OBJECT_TYPE, true);
			if (additionalTypeP.getUom().equals(UOM_NONE))
				throw new IllegalStateException(
						"Additional Type UOM " + additionalTypeP.getUom() + " invalid: " + additionalTypeP.getId()
								+ " for " + additionalTypeP.getLocator());
			this.filterCriteriaParams.put(additionalTypeP.getValue(), additionalTypeP);
		}
		if (this.reportRes.hasParameterBag(BAG_ADDITIONAL_JOINS)) {
			ParameterBag joinBag = this.reportRes.getParameterBag(BAG_ADDITIONAL_JOINS);
			joinBag.getParameters().forEach(parameter -> {
				StringParameter joinP = (StringParameter) parameter;
				if (joinP.getUom().equals(UOM_NONE))
					throw new IllegalStateException(
							"Additional Join UOM " + joinP.getUom() + " invalid: " + joinP.getId() + " for "
									+ joinP.getLocator());
				this.filterCriteriaParams.put(parameter.getId(), joinP);
			});
		}

		// evaluate ordering params
		if (this.reportRes.hasParameterBag(BAG_ORDERING)) {
			ParameterBag orderingBag = this.reportRes.getParameterBag(BAG_ORDERING, true);
			if (orderingBag.hasParameters()) {
				this.orderingParams = orderingBag.getParameters().stream().map(e -> (StringParameter) e)
						.collect(toList());
				this.orderingParams.sort(comparingInt(AbstractParameter::getIndex));
			}
		}

		// evaluate filters
		this.filtersByPolicy = new HashMap<>();
		List<ParameterBag> filterBags = this.reportRes.getParameterBagsByType(TYPE_FILTER);
		for (ParameterBag filterBag : filterBags) {

			if (filterBag.hasParameter(PARAM_FIELD_REF) && (filterBag.hasParameter(PARAM_FIELD_REF1)
					|| filterBag.hasParameter(PARAM_FIELD_REF2))) {
				throw new IllegalArgumentException(
						"Filter " + filterBag.getLocator() + " can not have combination of " + PARAM_FIELD_REF
								+ " and any of " + PARAM_FIELD_REF1 + ", " + PARAM_FIELD_REF2);
			} else if ((filterBag.hasParameter(PARAM_FIELD_REF1) && !filterBag.hasParameter(PARAM_FIELD_REF2)) || (
					!filterBag.hasParameter(PARAM_FIELD_REF1) && filterBag.hasParameter(PARAM_FIELD_REF2))) {
				throw new IllegalArgumentException(
						"Filter " + filterBag.getLocator() + " must have both " + PARAM_FIELD_REF1 + " and "
								+ PARAM_FIELD_REF2);
			} else if (!filterBag.hasParameter(PARAM_FIELD_REF) && (!filterBag.hasParameter(PARAM_FIELD_REF1)
					|| !filterBag.hasParameter(PARAM_FIELD_REF2))) {
				throw new IllegalArgumentException(
						"Filter " + filterBag.getLocator() + " is missing the " + PARAM_FIELD_REF + " or "
								+ PARAM_FIELD_REF1 + ", " + PARAM_FIELD_REF2 + " combination!");
			}

			// prepare filter function policy
			StringParameter functionP = filterBag.getParameter(PARAM_POLICY);
			PolicyHandler policyHandler = getContainer().getComponent(PolicyHandler.class);
			PolicyDef policyDef = PolicyDef.valueOf(functionP.getInterpretation(), functionP.getUom());
			ReportFilterPolicy filterFunction = policyHandler.getPolicy(policyDef, tx());
			filterFunction.init(functionP.getValue());

			TypedTuple<StringParameter, StringParameter> refTuple = new TypedTuple<>();
			if (filterBag.hasParameter(PARAM_FIELD_REF)) {
				refTuple.setFirst(filterBag.getParameter(PARAM_FIELD_REF));
			} else {
				refTuple.setFirst(filterBag.getParameter(PARAM_FIELD_REF1));
				refTuple.setSecond(filterBag.getParameter(PARAM_FIELD_REF2));
			}

			this.filtersByPolicy.put(filterFunction, refTuple);
		}
	}

	public boolean isDescending() {
		return this.descending;
	}

	@Override
	public boolean isParallel() {
		return this.parallel;
	}

	@Override
	public void setI18nData(JsonObject i18nData) {
		this.i18nData = i18nData;
	}

	@Override
	public boolean withPage() {
		return withPage;
	}

	@Override
	public int getOffset() {
		return this.offset;
	}

	@Override
	public int getLimit() {
		return this.limit;
	}

	@Override
	public long getCounter() {
		return this.counter;
	}

	/**
	 * Returns true if the report has a date range selector specified
	 *
	 * @return true if the report has a date range selector specified
	 */
	@Override
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
	@Override
	public GenericReport dateRange(DateRange dateRange) {
		this.dateRange = dateRange;
		return this;
	}

	/**
	 * Returns the currently set {@link DateRange} or null if not set
	 *
	 * @return the date range, or null if not set
	 */
	public DateRange getDateRange() {
		return this.dateRange;
	}

	/**
	 * The keys for the header of this report, as is defined on the {@link ReportConstants#BAG_COLUMNS} parameter bag
	 *
	 * @return the keys for the header
	 */
	@Override
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
	@Override
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
	@Override
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
	@Override
	public GenericReport filter(String type, Set<String> ids) {
		if (this.filtersById == null)
			this.filtersById = new MapOfSets<>();
		for (String id : ids) {
			this.filtersById.addElement(type, id);
		}
		return this;
	}

	protected synchronized void incrementCounter() {
		this.counter++;
	}

	/**
	 * Builds the stream of rows on which further transformations can be performed. Each row is a {@link Map} for where
	 * the key is an element type, and the value is the associated element
	 *
	 * @return this for chaining
	 */
	@Override
	public Stream<Map<String, StrolchRootElement>> buildStream() {

		Stream<Map<String, StrolchRootElement>> stream;

		// query the main objects and return a stream
		stream = queryRows() //

				// transform each element into a map of Type,Value pairs
				.map(this::evaluateRow);

		stream = handleAdditionalTypes(stream);

		stream = flatMap(stream);

		if (hasFilter())
			stream = stream.filter(this::filter);

		stream = stream.peek(e -> incrementCounter());

		if (hasOrdering())
			stream = stream.sorted(this::sort);

		return stream;
	}

	/**
	 * Allows sub classes to extend this stream, i.e. flat map an object to extend the stream where necessary
	 *
	 * @param stream
	 * 		the stream to extend
	 *
	 * @return the stream
	 */
	public Stream<Map<String, StrolchRootElement>> flatMap(Stream<Map<String, StrolchRootElement>> stream) {
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

		StringParameter objectTypeP = additionalTypeBag.getStringP(PARAM_OBJECT_TYPE);

		StringParameter joinParamP = additionalTypeBag.getStringP(PARAM_JOIN_PARAM);
		String[] locatorParts = joinParamP.getValue().split(Locator.PATH_SEPARATOR);
		if (locatorParts.length != 3)
			throw new IllegalStateException(
					"Parameter reference (" + joinParamP.getValue() + ") is invalid as it does not have 3 parts for "
							+ joinParamP.getLocator());
		String bagKey = locatorParts[1];
		String paramKey = locatorParts[2];

		MapOfLists<String, StrolchRootElement> joinElements = getStreamFor(objectTypeP).collect(MapOfLists::new,
				(mapOfLists, e) -> {
					StringParameter joinP = e.getParameter(bagKey, paramKey, true);
					mapOfLists.addElement(joinP.getValue(), e);
				}, MapOfLists::addAll);

		StringParameter joinWithP = additionalTypeBag.getStringP(PARAM_JOIN_WITH);
		return stream.flatMap(row -> {

			StrolchRootElement joinElement = row.get(joinWithP.getUom());
			if (joinElement == null)
				throw new IllegalStateException(
						"Additional join type " + joinWithP.getUom() + " is not available on row for "
								+ joinWithP.getLocator());

			Optional<Parameter<?>> refP = lookupParameter(joinWithP, joinElement);
			if (refP.isEmpty()) {
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
	@Override
	public Stream<ReportElement> doReport() {
		return buildStream().map(e -> new ReportElement(this.columnIds, columnId -> {
			StringParameter columnDefP = this.columnsBag.getParameter(columnId, true);
			Object value = evaluateColumnValue(columnDefP, e, false);
			if (value instanceof ZonedDateTime)
				return ISO8601.toString((ZonedDateTime) value);
			if (value instanceof Date)
				return ISO8601.toString((Date) value);
			if (value instanceof Parameter)
				return ((Parameter<?>) value).getValueAsString();
			return value.toString();
		}));
	}

	@Override
	public Stream<ReportElement> doReportWithPage(int offset, int limit) {
		DBC.PRE.assertTrue("offset must >= 0", offset >= 0);
		DBC.PRE.assertTrue("limit must > 0", limit > 0);
		this.limit = limit;
		this.offset = offset;
		this.withPage = true;

		return doReport().skip(this.offset).limit(this.limit);
	}

	/**
	 * <p>Check to see if the given element is to be filtered, i.e. is to be kept in the stream</p>
	 *
	 * <p>This implementation checks if a join parameters is set as hidden, including if the parameter {@link
	 * ReportConstants#PARAM_OBJECT_TYPE} is defined as hidden</p>
	 *
	 * <p>This method can be overridden for further filtering</p>
	 *
	 * @param type
	 * 		the type of element to filter
	 *
	 * @return true if the element is to be kept, false if not
	 */
	protected boolean filterCriteriaAllowed(String type) {
		return !this.filterCriteriaParams.get(type).isHidden();
	}

	/**
	 * Generates the filter criteria for this report, i.e. it returns a {@link MapOfSets} which defines the type of
	 * elements on which a filter can be set and the {@link Set} of IDs which can be used for filtering.
	 *
	 * @return the filter criteria as a map of sets
	 */
	@Override
	public MapOfSets<String, StrolchRootElement> generateFilterCriteria(int limit) {

		MapOfSets<String, StrolchRootElement> result = new MapOfSets<>(true);

		Iterator<Map<String, StrolchRootElement>> iter = buildStream().iterator();
		if (!iter.hasNext())
			return result;

		List<String> criteria = this.filterCriteriaParams.values().stream() //
				.filter(p -> {
					if (p.getUom().equals(UOM_NONE))
						throw new IllegalStateException(
								"Join UOM " + p.getUom() + " invalid: " + p.getId() + " for " + p.getLocator());
					if (p.getId().equals(PARAM_OBJECT_TYPE))
						return filterCriteriaAllowed(p.getUom());
					return filterCriteriaAllowed(p.getId());
				}) //
				.sorted(comparing(StringParameter::getIndex)) //
				.map(StringParameter::getUom).collect(toList());

		int count = 0;
		while (iter.hasNext()) {
			Map<String, StrolchRootElement> row = iter.next();

			for (String criterion : criteria) {
				if (row.containsKey(criterion)) {
					if (result.size(criterion) >= limit)
						continue;

					result.addElement(criterion, row.get(criterion));
				}
			}

			// stop if we have enough data, or iterated over "enough"
			count++;
			if (count > 1000 || result.size() > limit * result.keySet().size())
				break;
		}

		return result;
	}

	@Override
	public Stream<StrolchRootElement> generateFilterCriteria(String type) {
		return buildStream().filter(row -> row.containsKey(type)).map(row -> row.get(type)).distinct();
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
				Object columnValue1 = evaluateColumnValue(fieldRefP, row1, false);
				Object columnValue2 = evaluateColumnValue(fieldRefP, row2, false);

				if (this.descending) {
					sortVal = ObjectHelper.compare(columnValue2, columnValue1, true);
				} else {
					sortVal = ObjectHelper.compare(columnValue1, columnValue2, true);
				}

			} else {
				Optional<Parameter<?>> param1 = lookupParameter(fieldRefP, column1);
				Optional<Parameter<?>> param2 = lookupParameter(fieldRefP, column2);

				if (param1.isEmpty() && param2.isEmpty())
					continue;

				if (param1.isPresent() && param2.isEmpty())
					return 1;
				else if (param1.isEmpty())
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
			TypedTuple<StringParameter, StringParameter> refTuple = this.filtersByPolicy.get(filterPolicy);

			if (refTuple.hasFirst() && refTuple.hasSecond()) {
				Object value1 = evaluateColumnValue(refTuple.getFirst(), row, true);
				Object value2 = evaluateColumnValue(refTuple.getSecond(), row, true);

				if (this.filterMissingValuesAsTrue && (value1 == null || value2 == null))
					continue;
				if (value1 == null || value2 == null || !filterPolicy.filter(value1, value2))
					return false;

			} else {
				Object value = evaluateColumnValue(refTuple.getFirst(), row, true);
				if (this.filterMissingValuesAsTrue && value == null)
					continue;
				if (value == null || !filterPolicy.filter(value))
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

			ZonedDateTime date;
			if (dateRangeSel.equals(COL_DATE)) {
				date = element.accept(new ElementZdtDateVisitor());
			} else {
				Optional<Parameter<?>> param = lookupParameter(this.dateRangeSelP, element);
				if (param.isEmpty() || param.get().getValueType() != StrolchValueType.DATE)
					throw new IllegalStateException(
							"Date Range selector is invalid, as referenced parameter is not a Date but "
									+ (param.isPresent() ? param.get().getValueType() : "null"));

				date = ((DateParameter) param.get()).getValueZdt();
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
	 * @param allowNull
	 * 		handles the return value if the lookup fails. If true, then null is returned, else the empty string is
	 * 		returned
	 *
	 * @return the column value
	 */
	protected Object evaluateColumnValue(StringParameter columnDefP, Map<String, StrolchRootElement> row,
			boolean allowNull) {

		String columnDef = columnDefP.getValue();
		String refType = columnDefP.getUom();

		// get the referenced object
		StrolchRootElement column = row.get(refType);

		Object columnValue;

		if (column == null) {
			columnValue = allowNull ? null : EMPTY;
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
			columnValue = column.accept(new ElementZdtDateVisitor());
		} else if (columnDef.startsWith(COL_SEARCH)) {
			Parameter<?> parameter = findParameter(columnDefP, column);
			if (parameter == null)
				columnValue = EMPTY;
			else
				columnValue = parameter;
		} else {
			columnValue = lookupParameter(columnDefP, column).orElseGet(() -> allowNull ? null : new StringParameter());
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
					"Parameter reference (" + paramRef + ") is invalid as it does not have 3 parts for "
							+ paramRefP.getLocator());

		String bagKey = locatorParts[1];
		String paramKey = locatorParts[2];

		Parameter<?> param = element.getParameter(bagKey, paramKey);
		if (!allowMissingColumns && param == null)
			throw new IllegalStateException(
					"Parameter reference (" + paramRef + ") for " + paramRefP.getLocator() + " not found on "
							+ element.getLocator());

		return Optional.ofNullable(param);
	}

	/**
	 * Returns a stream of {@link StrolchRootElement} which denote the rows of the report. This implementation uses
	 * {@link ReportConstants#PARAM_OBJECT_TYPE} to stream the initial rows
	 *
	 * @return the stream of {@link StrolchRootElement StrolchRootElement}
	 */
	protected Stream<? extends StrolchRootElement> queryRows() {
		if (isParallel())
			return getStreamFor(getObjectTypeParam()).parallel();
		return getStreamFor(getObjectTypeParam());
	}

	protected StringParameter getObjectTypeParam() {
		return this.reportRes.getStringP(BAG_PARAMETERS, PARAM_OBJECT_TYPE);
	}

	protected String getObjectType() {
		return getObjectTypeParam().getValue();
	}

	protected boolean hasJoinOnType(String type) {
		return (this.reportRes.hasParameterBag(BAG_JOINS) //
				&& this.reportRes.getParameterBag(BAG_JOINS).hasParameter(type)) //
				|| (this.reportRes.hasParameterBag(BAG_ADDITIONAL_TYPE) //
				&& this.reportRes.getParameterBag(BAG_ADDITIONAL_TYPE).getString(PARAM_OBJECT_TYPE).equals(type)) //
				|| (this.reportRes.hasParameterBag(BAG_ADDITIONAL_JOINS) //
				&& this.reportRes.getParameterBag(BAG_ADDITIONAL_JOINS).hasParameter(type));
	}

	private Stream<? extends StrolchRootElement> getStreamFor(StringParameter objectTypeP) {
		switch (objectTypeP.getInterpretation()) {
		case INTERPRETATION_RESOURCE_REF:
			return tx().streamResources(objectTypeP.getUom());
		case INTERPRETATION_ORDER_REF:
			return tx().streamOrders(objectTypeP.getUom());
		case INTERPRETATION_ACTIVITY_REF:
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

		// now add all the joins
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
			if (dependencyP == null)
				throw new IllegalStateException(
						"The defined join dependency " + dependencyType + " does not exist for " + joinP.getLocator());
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
				.stream().filter(p -> p.getValueType() == StrolchValueType.STRING).collect(toList());

		if (relationParams.isEmpty())
			throw new IllegalStateException("Found no relation parameters with UOM " + joinType + " of type "
					+ StrolchValueType.STRING.getType() + " on dependency " + dependency.getLocator());
		if (relationParams.size() > 1)
			throw new IllegalStateException(
					"Found multiple possible relation parameters for UOM " + joinType + " on dependency "
							+ dependency.getLocator());

		Parameter<?> relationParam = relationParams.get(0);
		StringParameter relationP = (StringParameter) relationParam;
		if (relationP.getValue().isEmpty() && optional)
			return null;

		Locator locator = Locator.valueOf(elementType, joinType, relationP.getValue());
		StrolchRootElement joinElem = tx().findElement(locator, true);
		if (joinElem == null)
			return null;

		refs.put(joinType, joinElem);
		return joinElem;
	}
}
