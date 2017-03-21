package li.strolch.report;

import static li.strolch.utils.helper.StringHelper.DASH;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.google.gson.JsonObject;

import li.strolch.model.Locator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.utils.collections.MapOfSets;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class GenericReport {

	private static final String BAG_RELATIONS = "relations";
	private static final String SUFFIX_REF = "-Ref";
	private static final String BAG_JOINS = "joins";
	private static final String PARAM_OBJECT_TYPE = "objectType";
	private static final String BAG_PARAMETERS = "parameters";
	private static final String COL_NAME = "$name";
	private static final String BAG_COLUMNS = "columns";
	private static final String TYPE_REPORT = "Report";

	// input
	private StrolchTransaction tx;
	private String reportId;
	private MapOfSets<String, String> filtersByType;

	// intermediate
	private Resource report;
	private ParameterBag columnsBag;

	public GenericReport(StrolchTransaction tx, String reportId) {
		this.tx = tx;
		this.reportId = reportId;
		this.filtersByType = new MapOfSets<>();
	}

	public GenericReport filter(String type, String... ids) {
		for (String id : ids) {
			this.filtersByType.addElement(type, id);
		}
		return this;
	}

	public GenericReport filter(String type, List<String> ids) {
		for (String id : ids) {
			this.filtersByType.addElement(type, id);
		}
		return this;
	}

	private Stream<Map<String, StrolchRootElement>> doReport() {

		// get the report
		this.report = this.tx.getResourceBy(TYPE_REPORT, this.reportId);

		this.columnsBag = this.report.getParameterBag(BAG_COLUMNS);

		// query the main objects and return a stream
		Stream<Map<String, StrolchRootElement>> stream = queryRows().map(e -> evaluateRow(e));

		if (!this.filtersByType.isEmpty())
			stream = stream.filter(e -> filter(e));

		return stream;
	}

	private boolean filter(Map<String, StrolchRootElement> row) {

		for (String type : this.filtersByType.keySet()) {
			StrolchRootElement element = row.get(type);
			if (element == null)
				return false;

			if (!this.filtersByType.getSet(type).contains(element.getId()))
				return false;
		}

		return true;
	}

	public Stream<JsonObject> doReportAsJson() {

		// generate the stream and map to JsonObject
		return doReport().map(row -> {

			// new json object
			JsonObject jsonObject = new JsonObject();

			// add columns
			evaluateColumns(row).forEach(e -> jsonObject.addProperty(e.getKey(), e.getValue()));

			return jsonObject;
		});
	}

	private Stream<SimpleImmutableEntry<String, String>> evaluateColumns(Map<String, StrolchRootElement> row) {

		// get iterator to columns
		Iterable<Parameter<?>> iterable = () -> this.columnsBag.getParameters().iterator();

		// generate a stream
		return StreamSupport.stream(iterable.spliterator(), false).map(p -> {

			// create the column id/value pair

			StringParameter columnDefP = (StringParameter) p;

			String columnId = columnDefP.getId();
			String refType = columnDefP.getUom();
			String columnDef = columnDefP.getValue();

			// get the referenced object
			StrolchRootElement column = row.get(refType);

			String columnValue;
			if (column == null) {
				columnValue = DASH;
			} else if (columnDef.equals(COL_NAME)) {
				columnValue = column.getName();
			} else {
				String[] locatorParts = columnDef.split(Locator.PATH_SEPARATOR);
				if (locatorParts.length != 3)
					throw new IllegalStateException(
							"Column definition is invalid as column must either be $name, or a three part parameter locator ");

				String bagKey = locatorParts[1];
				String paramKey = locatorParts[2];

				Parameter<?> param = column.getParameter(bagKey, paramKey);
				columnValue = param.getValueAsString();
			}

			return new SimpleImmutableEntry<>(columnId, columnValue);
		});
	}

	private Stream<StrolchRootElement> queryRows() {

		// find the type of object for which the report is created
		StringParameter objectTypeP = this.report.getParameter(BAG_PARAMETERS, PARAM_OBJECT_TYPE);

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
		ParameterBag joinBag = this.report.getParameterBag(BAG_JOINS);

		// create the refs element
		HashMap<String, StrolchRootElement> refs = new HashMap<>();
		// and add the starting point
		refs.put(resource.getType(), resource);

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
		}

		ParameterBag relationsBag = dependency.getParameterBag(BAG_RELATIONS);
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
			refs.put(joinType, null);
			return null;
		}

		Locator locator = Locator.valueOf(elementType, joinType, relationP.getValue());
		StrolchRootElement joinElem;
		try {
			joinElem = this.tx.findElement(locator);
		} catch (Exception e) {
			throw new IllegalStateException("Failed to find join element " + joinType + " for dependency "
					+ dependency.getLocator() + " with locator " + locator);
		}

		refs.put(joinType, joinElem);
		return joinElem;
	}
}
