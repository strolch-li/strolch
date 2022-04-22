package ${package}.model;

import static ${package}.model.Constants.*;

import java.util.Optional;

import com.google.gson.JsonArray;
import li.strolch.model.StrolchElement;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;

public class JsonVisitors {

	public static StrolchRootElementToJsonVisitor toJson() {
		return new StrolchRootElementToJsonVisitor().withoutPolicies();
	}

	public static StrolchRootElementToJsonVisitor flatToJson() {
		return toJson().withoutVersion().flat();
	}

	public static StrolchRootElementToJsonVisitor bookToJson() {
		return flatToJson();
	}

	public static StrolchRootElementToJsonVisitor stockBagToJson(StrolchTransaction tx) {
		return flatToJson().bagHook((stockBag, stockJ) -> stockJ.addProperty(PARAM_BOOK_NAME,
				Optional.ofNullable(tx.getResourceBy(stockBag.getParameter(PARAM_BOOK), false))
						.map(StrolchElement::getName).orElse("???")));
	}

	public static StrolchRootElementToJsonVisitor locationToJson(StrolchTransaction tx) {
		StrolchRootElementToJsonVisitor stockVisitor = stockBagToJson(tx);
		return flatToJson().ignoreBagByType(TYPE_STOCK).resourceHook(
				(location, locationJ) -> locationJ.add(PARAM_STOCKS,
						location.streamOfParameterBagsByType(TYPE_STOCK).map(b -> b.accept(stockVisitor))
								.collect(JsonArray::new, JsonArray::add, JsonArray::addAll)));
	}
}
