package li.strolch.report;

import static li.strolch.utils.helper.StringHelper.DASH;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.gson.JsonObject;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Locator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.StrolchValueType;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.visitor.ElementDateVisitor;
import li.strolch.model.visitor.ElementStateVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.report.policy.ReportFilterPolicy;
import li.strolch.runtime.StrolchConstants;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class GenericReport {

	private static final String TYPE_REPORT = "Report";
	private static final String TYPE_FILTER = "Filter";

	private static final String BAG_RELATIONS = "relations";
	private static final String BAG_JOINS = "joins";
	private static final String BAG_PARAMETERS = "parameters";
	private static final String BAG_COLUMNS = "columns";

	private static final String PARAM_OBJECT_TYPE = "objectType";
	private static final String PARAM_HIDE_OBJECT_TYPE_FROM_FILTER_CRITERIA = "hideObjectTypeFromFilterCriteria";
	private static final String PARAM_DATE_RANGE_SEL = "dateRangeSel";
	private static final String PARAM_FIELD_REF = "fieldRef";
	private static final String PARAM_POLICY = "policy";

	private static final String COL_ID = "$id";
	private static final String COL_NAME = "$name";
	private static final String COL_TYPE = "$type";
	private static final String COL_STATE = "$state";
	private static final String COL_DATE = "$date";
	private static final String COL_SEARCH = "$search";

	private static final String SEARCH_SEPARATOR = ":";

	private static final String SUFFIX_REF = "-Ref";

	// input
	private StrolchTransaction tx;
	private String reportId;

	private Resource report;
	private StringParameter objectTypeP;
	private boolean hideObjectTypeFromFilterCriteria;
	private ParameterBag columnsBag;
	private Set<String> columnIds;
	private StringParameter dateRangeSelP;

	private DateRange dateRange;
	private Map<ReportFilterPolicy, StringParameter> filtersByPolicy;
	private MapOfSets<String, String> filtersById;

	public GenericReport(ComponentContainer container, StrolchTransaction tx, String reportId) {
		this.tx = tx;
		this.reportId = reportId;

		// get the report
		this.report = this.tx.getResourceBy(TYPE_REPORT, this.reportId, true);

		// prepare
		this.objectTypeP = this.report.getParameter(BAG_PARAMETERS, PARAM_OBJECT_TYPE);

		BooleanParameter hideObjectTypeFromFilterCriteriaP = this.report.getParameter(BAG_PARAMETERS,
				PARAM_HIDE_OBJECT_TYPE_FROM_FILTER_CRITERIA);
		this.hideObjectTypeFromFilterCriteria = hideObjectTypeFromFilterCriteriaP != null
				&& hideObjectTypeFromFilterCriteriaP.getValue();
		this.columnsBag = this.report.getParameterBag(BAG_COLUMNS, true);
		this.columnIds = this.columnsBag.getParameterKeySet();
		this.dateRangeSelP = this.report.getParameter(BAG_PARAMETERS, PARAM_DATE_RANGE_SEL);

		// evaluate filters
		this.filtersByPolicy = new HashMap<>();
		List<ParameterBag> filterBags = this.report.getParameterBagsByType(TYPE_FILTER);
		for (ParameterBag filterBag : filterBags) {

			// prepare filter function policy
			StringParameter functionP = filterBag.getParameter(PARAM_POLICY);
			PolicyHandler policyHandler = container.getComponent(PolicyHandler.class);
			PolicyDef policyDef = PolicyDef.valueOf(functionP.getInterpretation(), functionP.getUom());
			ReportFilterPolicy filterFunction = policyHandler.getPolicy(policyDef, tx);
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

	public List<String> getOrderedColumnKeys() {
		return this.columnsBag.getParameters().stream() //
				.sorted((p1, p2) -> Integer.compare(p1.getIndex(), p2.getIndex())) //
				.map(p -> p.getId()) //
				.collect(Collectors.toList());
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
		return queryRows().map(e -> evaluateRow(e)).filter(e -> filter(e));
	}

	public Stream<ReportElement> doReport() {

		return buildStream().map(e -> new ReportElement(this.columnIds, columnId -> {
			StringParameter columnDefP = this.columnsBag.getParameter(columnId, true);
			return evaluateColumnValue(columnDefP, e);
		}));
	}

	public MapOfSets<String, StrolchRootElement> generateFilterCriteria() {
		return buildStream() //
				.flatMap(e -> {
					Stream<StrolchRootElement> stream = e.values().stream();
					if (this.hideObjectTypeFromFilterCriteria) {
						stream = stream.filter(element -> !element.getType().equals(this.objectTypeP.getValue()));
					}
					return stream;
				}) //
				.collect( //
						Collector.of( //
								() -> new MapOfSets<String, StrolchRootElement>(), //
								(m, e) -> m.addElement(e.getType(), e), //
								(m1, m2) -> m1.addAll(m2), //
								m -> m));
	}

	private boolean filter(Map<String, StrolchRootElement> row) {

		// do filtering by policies
		for (ReportFilterPolicy filterPolicy : this.filtersByPolicy.keySet()) {
			StringParameter fieldRefP = this.filtersByPolicy.get(filterPolicy);

			String type = fieldRefP.getUom();

			StrolchRootElement column = row.get(type);

			if (fieldRefP.getValue().startsWith("$")) {
				String columnValue = evaluateColumnValue(fieldRefP, row);
				if (!filterPolicy.filter(columnValue))
					return false;
			} else {
				Parameter<?> param = lookupParameter(fieldRefP, column);
				if (!filterPolicy.filter(param))
					return false;
			}
		}

		// do a date range selection, if required
		if (this.dateRange != null) {
			if (this.dateRangeSelP == null)
				throw new IllegalStateException(
						"DateRange defined, but report does not defined a date range selector!");

			String type = this.dateRangeSelP.getUom();
			StrolchRootElement element = row.get(type);
			if (element == null)
				return false;

			String dateRangeSel = this.dateRangeSelP.getValue();

			Date date;
			if (dateRangeSel.equals(COL_DATE)) {
				date = element.accept(new ElementDateVisitor());
			} else {
				Parameter<?> param = lookupParameter(this.dateRangeSelP, element);
				if (StrolchValueType.parse(param.getType()) != StrolchValueType.DATE)
					throw new IllegalStateException(
							"Date Range selector is invalid, as referenced parameter is not a Date but a "
									+ param.getType());

				date = ((DateParameter) param).getValue();
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

	public Stream<JsonObject> doReportAsJson() {

		return doReport().map(e -> {
			JsonObject o = new JsonObject();
			e.keyValueStream().forEach(elem -> o.addProperty(elem.getKey(), elem.getValue()));
			return o;
		});
	}

	private String evaluateColumnValue(StringParameter columnDefP, Map<String, StrolchRootElement> row) {

		String columnDef = columnDefP.getValue();
		String refType = columnDefP.getUom();

		// get the referenced object
		StrolchRootElement column = row.get(refType);

		String columnValue;

		if (column == null)
			columnValue = DASH;
		else if (columnDef.equals(COL_ID)) {
			columnValue = column.getId();
		} else if (columnDef.equals(COL_NAME)) {
			columnValue = column.getName();
		} else if (columnDef.equals(COL_TYPE)) {
			columnValue = column.getType();
		} else if (columnDef.equals(COL_STATE)) {
			columnValue = column.accept(new ElementStateVisitor()).name();
		} else if (columnDef.equals(COL_DATE)) {
			columnValue = ISO8601FormatFactory.getInstance().formatDate(column.accept(new ElementDateVisitor()));
		} else if (columnDef.startsWith(COL_SEARCH)) {
			Parameter<?> parameter = findParameter(columnDefP, column);
			if (parameter == null)
				columnValue = DASH;
			else
				columnValue = parameter.getValueAsString();
		} else {
			columnValue = lookupParameter(columnDefP, column).getValueAsString();
		}

		return columnValue;
	}

	private Parameter<?> findParameter(StringParameter columnDefP, StrolchRootElement column) {

		String columnDef = columnDefP.getValue();

		String[] searchParts = columnDef.split(SEARCH_SEPARATOR);
		if (searchParts.length != 3)
			throw new IllegalStateException("Parameter search reference (" + columnDef
					+ ") is invalid as it does not have 3 parts for " + columnDefP.getLocator());

		String parentParamId = searchParts[1];
		String paramRef = searchParts[2];

		String[] locatorParts = paramRef.split(Locator.PATH_SEPARATOR);
		if (locatorParts.length != 3)
			throw new IllegalStateException("Parameter search reference (" + paramRef
					+ ") is invalid as it does not have 3 parts for " + columnDefP.getLocator());

		String bagKey = locatorParts[1];
		String paramKey = locatorParts[2];

		return findParameter(column, parentParamId, bagKey, paramKey);
	}

	private Parameter<?> findParameter(StrolchRootElement element, String parentParamId, String bagKey,
			String paramKey) {

		Parameter<?> parameter = element.getParameter(bagKey, paramKey);
		if (parameter != null)
			return parameter;

		StringParameter parentRefP = element.getParameter(BAG_RELATIONS, parentParamId);
		if (parentRefP == null)
			return null;

		Resource parent = this.tx.getResourceBy(parentRefP);
		if (parent == null)
			return null;

		return findParameter(parent, parentParamId, bagKey, paramKey);
	}

	private Parameter<?> lookupParameter(StringParameter paramRefP, StrolchRootElement column) {
		String paramRef = paramRefP.getValue();

		String[] locatorParts = paramRef.split(Locator.PATH_SEPARATOR);
		if (locatorParts.length != 3)
			throw new IllegalStateException("Parameter reference (" + paramRef
					+ ") is invalid as it does not have 3 parts for " + paramRefP.getLocator());

		String bagKey = locatorParts[1];
		String paramKey = locatorParts[2];

		Parameter<?> param = column.getParameter(bagKey, paramKey);
		if (param == null)
			throw new IllegalStateException("Parameter reference (" + paramRef + ") for " + paramRefP.getLocator()
					+ " not found on " + column.getLocator());

		return param;
	}

	private Stream<StrolchRootElement> queryRows() {

		// find the type of object for which the report is created
		if (objectTypeP.getInterpretation().equals(StrolchConstants.INTERPRETATION_RESOURCE_REF)) {

			return this.tx.getResourceMap().getElementsBy(this.tx, objectTypeP.getUom()).stream()
					.map(e -> e.getRootElement());

		} else if (objectTypeP.getInterpretation().equals(StrolchConstants.INTERPRETATION_ORDER_REF)) {

			return this.tx.getOrderMap().getElementsBy(this.tx, objectTypeP.getUom()).stream()
					.map(e -> e.getRootElement());

		} else {

			throw new IllegalArgumentException("Unhandled element type " + objectTypeP.getInterpretation());
		}
	}

	private Map<String, StrolchRootElement> evaluateRow(StrolchRootElement resource) {

		// interpretation -> Resource-Ref, etc.
		// uom -> object type
		// value -> element type where relation is defined for this join

		// create the refs element
		HashMap<String, StrolchRootElement> refs = new HashMap<>();
		// and add the starting point
		refs.put(resource.getType(), resource);

		if (!this.report.hasParameterBag(BAG_JOINS))
			return refs;

		ParameterBag joinBag = this.report.getParameterBag(BAG_JOINS);
		for (String paramId : joinBag.getParameterKeySet()) {
			StringParameter joinP = joinBag.getParameter(paramId);
			addColumnJoin(refs, joinBag, joinP, true);
		}

		return refs;
	}

	private StrolchRootElement addColumnJoin(Map<String, StrolchRootElement> refs, ParameterBag joinBag,
			StringParameter joinP, boolean optional) {

		String elementType = joinP.getInterpretation().substring(0, joinP.getInterpretation().indexOf(SUFFIX_REF));
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
			throw new IllegalStateException("Invalid join definition value: " + joinP.getValue() + " on: "
					+ joinP.getLocator() + " as " + dependency.getLocator() + " has no ParameterBag " + BAG_RELATIONS);

		List<Parameter<?>> relationParams = relationsBag.getParameters().stream()
				.filter(p -> p.getUom().equals(joinType)).collect(Collectors.toList());
		if (relationParams.isEmpty()) {
			throw new IllegalStateException(
					"Found no relation parameters with UOM " + joinType + " on dependency " + dependency.getLocator());
		}
		if (relationParams.size() > 1) {
			throw new IllegalStateException("Found multiple possible relation parameters for UOM " + joinType
					+ " on dependency " + dependency.getLocator());
		}

		StringParameter relationP = (StringParameter) relationParams.get(0);
		if (relationP.getValue().isEmpty() && optional) {
			return null;
		}

		Locator locator = Locator.valueOf(elementType, joinType, relationP.getValue());
		StrolchRootElement joinElem = this.tx.findElement(locator, true);
		if (joinElem == null)
			return null;

		refs.put(joinType, joinElem);
		return joinElem;
	}
}
