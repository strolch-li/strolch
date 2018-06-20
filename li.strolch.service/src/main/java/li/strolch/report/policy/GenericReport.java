package li.strolch.report.policy;

import static java.util.Comparator.comparingInt;
import static li.strolch.report.ReportConstants.*;
import static li.strolch.utils.helper.StringHelper.EMPTY;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.*;
import li.strolch.model.parameter.*;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.visitor.ElementDateVisitor;
import li.strolch.model.visitor.ElementStateVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.report.ReportElement;
import li.strolch.runtime.StrolchConstants;
import li.strolch.utils.ObjectHelper;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class GenericReport extends ReportPolicy {

	private Resource reportRes;

	private boolean hideObjectTypeFromFilterCriteria;
	private String objectType;

	private ParameterBag columnsBag;
	private List<StringParameter> orderingParams;
	private boolean descending;
	private boolean allowMissingColumns;
	private List<String> columnIds;
	private StringParameter dateRangeSelP;

	private DateRange dateRange;
	private Map<ReportFilterPolicy, StringParameter> filtersByPolicy;
	private MapOfSets<String, String> filtersById;

	public GenericReport(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public void initialize(String reportId) {

		// get the reportRes
		this.reportRes = tx().getResourceBy(TYPE_REPORT, reportId, true);

		BooleanParameter hideObjectTypeFromFilterCriteriaP = this.reportRes
				.getParameter(BAG_PARAMETERS, PARAM_HIDE_OBJECT_TYPE_FROM_FILTER_CRITERIA);
		this.hideObjectTypeFromFilterCriteria =
				hideObjectTypeFromFilterCriteriaP != null && hideObjectTypeFromFilterCriteriaP.getValue();
		if (this.hideObjectTypeFromFilterCriteria) {
			this.objectType = this.reportRes.getParameter(BAG_PARAMETERS, PARAM_OBJECT_TYPE).getValue();
		}

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

		// evaluate ordering params
		if (this.reportRes.hasParameterBag(BAG_ORDERING)) {
			ParameterBag orderingBag = this.reportRes.getParameterBag(BAG_ORDERING);
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

	public boolean hasDateRangeSelector() {
		return this.dateRangeSelP != null;
	}

	public GenericReport dateRange(DateRange dateRange) {
		this.dateRange = dateRange;
		return this;
	}

	public List<String> getColumnKeys() {
		return this.columnIds;
	}

	public GenericReport filter(String type, String... ids) {
		if (this.filtersById == null)
			this.filtersById = new MapOfSets<>();
		for (String id : ids) {
			this.filtersById.addElement(type, id);
		}
		return this;
	}

	public GenericReport filter(String type, List<String> ids) {
		if (this.filtersById == null)
			this.filtersById = new MapOfSets<>();
		for (String id : ids) {
			this.filtersById.addElement(type, id);
		}
		return this;
	}

	public GenericReport filter(String type, Set<String> ids) {
		if (this.filtersById == null)
			this.filtersById = new MapOfSets<>();
		for (String id : ids) {
			this.filtersById.addElement(type, id);
		}
		return this;
	}

	public Stream<Map<String, StrolchRootElement>> buildStream() {

		// query the main objects and return a stream
		Stream<Map<String, StrolchRootElement>> stream = queryRows().map(this::evaluateRow);

		if (hasFilter())
			stream = stream.filter(this::filter);

		if (hasOrdering())
			stream = stream.sorted(this::sort);

		return stream;
	}

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

	protected boolean filterCriteria(StrolchRootElement element) {
		if (this.hideObjectTypeFromFilterCriteria)
			return !element.getType().equals(this.objectType);
		return true;
	}

	public MapOfSets<String, StrolchRootElement> generateFilterCriteria() {
		return buildStream() //

				.flatMap(e -> e.values().stream().filter(this::filterCriteria)) //

				.collect( //
						Collector.of( //
								(Supplier<MapOfSets<String, StrolchRootElement>>) MapOfSets::new, //
								(m, e) -> m.addElement(e.getType(), e), //
								MapOfSets::addAll, //
								m -> m));
	}

	protected boolean hasOrdering() {
		return this.orderingParams != null && !this.orderingParams.isEmpty();
	}

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
					sortVal = ObjectHelper.compare(columnValue1, columnValue2, true);
				} else {
					sortVal = ObjectHelper.compare(columnValue2, columnValue1, true);
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
					sortVal = param1.get().compareTo(param2.get());
				else
					sortVal = param2.get().compareTo(param1.get());
			}

			if (sortVal != 0)
				return sortVal;
		}

		return 0;
	}

	protected boolean hasFilter() {
		return !this.filtersByPolicy.isEmpty() || this.dateRange != null || (this.filtersById != null
				&& !this.filtersById.isEmpty());
	}

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

				String columnValue;
				if (value instanceof Date)
					columnValue = ISO8601FormatFactory.getInstance().formatDate((Date) value);
				else if (value instanceof Parameter)
					columnValue = ((Parameter) value).getValueAsString();
				else
					columnValue = value.toString();

				if (!filterPolicy.filter(columnValue))
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

	protected Object evaluateColumnValue(StringParameter columnDefP, Map<String, StrolchRootElement> row) {

		String columnDef = columnDefP.getValue();
		String refType = columnDefP.getUom();

		// get the referenced object
		StrolchRootElement column = row.get(refType);

		Object columnValue;

		if (column == null)
			columnValue = EMPTY;
		else if (columnDef.equals(COL_ID)) {
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

	protected Parameter<?> findParameter(StringParameter columnDefP, StrolchRootElement column) {

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

		return findParameter(column, parentParamId, bagKey, paramKey);
	}

	protected Parameter<?> findParameter(StrolchRootElement element, String parentParamId, String bagKey,
			String paramKey) {

		Parameter<?> parameter = element.getParameter(bagKey, paramKey);
		if (parameter != null)
			return parameter;

		StringParameter parentRefP = element.getParameter(BAG_RELATIONS, parentParamId);
		if (parentRefP == null)
			return null;

		Resource parent = tx().getResourceBy(parentRefP);
		if (parent == null)
			return null;

		return findParameter(parent, parentParamId, bagKey, paramKey);
	}

	protected Optional<Parameter<?>> lookupParameter(StringParameter paramRefP, StrolchRootElement column) {
		String paramRef = paramRefP.getValue();

		String[] locatorParts = paramRef.split(Locator.PATH_SEPARATOR);
		if (locatorParts.length != 3)
			throw new IllegalStateException(
					"Parameter reference (" + paramRef + ") is invalid as it does not have 3 parts for " + paramRefP
							.getLocator());

		String bagKey = locatorParts[1];
		String paramKey = locatorParts[2];

		Parameter<?> param = column.getParameter(bagKey, paramKey);
		if (!allowMissingColumns && param == null)
			throw new IllegalStateException(
					"Parameter reference (" + paramRef + ") for " + paramRefP.getLocator() + " not found on " + column
							.getLocator());

		return Optional.ofNullable(param);
	}

	protected Stream<? extends StrolchRootElement> queryRows() {

		StringParameter objectTypeP = this.reportRes.getParameter(BAG_PARAMETERS, PARAM_OBJECT_TYPE);

		// find the type of object for which the reportRes is created
		switch (objectTypeP.getInterpretation()) {

		case StrolchConstants.INTERPRETATION_RESOURCE_REF:

			return tx().getResourceMap().getElementsBy(tx(), objectTypeP.getUom()).stream();

		case StrolchConstants.INTERPRETATION_ORDER_REF:

			return tx().getOrderMap().getElementsBy(tx(), objectTypeP.getUom()).stream();

		default:

			throw new IllegalArgumentException("Unhandled element type " + objectTypeP.getInterpretation());
		}
	}

	protected Map<String, StrolchRootElement> evaluateRow(StrolchRootElement resource) {

		// interpretation -> Resource-Ref, etc.
		// uom -> object type
		// value -> element type where relation is defined for this join

		// create the refs element
		HashMap<String, StrolchRootElement> refs = new HashMap<>();
		// and add the starting point
		refs.put(resource.getType(), resource);

		if (!this.reportRes.hasParameterBag(BAG_JOINS))
			return refs;

		ParameterBag joinBag = this.reportRes.getParameterBag(BAG_JOINS);
		for (String paramId : joinBag.getParameterKeySet()) {
			StringParameter joinP = joinBag.getParameter(paramId);
			addColumnJoin(refs, joinBag, joinP, true);
		}

		return refs;
	}

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

		List<Parameter<?>> relationParams = relationsBag.getParametersByInterpretationAndUom(interpretation, joinType);

		if (relationParams.isEmpty()) {
			throw new IllegalStateException(
					"Found no relation parameters with UOM " + joinType + " on dependency " + dependency.getLocator());
		}
		if (relationParams.size() > 1) {
			throw new IllegalStateException(
					"Found multiple possible relation parameters for UOM " + joinType + " on dependency " + dependency
							.getLocator());
		}

		Parameter<?> relationParam = relationParams.get(0);
		if (!relationParam.getType().equals(StrolchValueType.STRING.getType())) {
			throw new IllegalStateException(
					"Expect to find relation parameter of type " + StrolchValueType.STRING.getType() + " but found "
							+ relationParam.getType() + " for " + relationParam.getLocator());
		}

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
