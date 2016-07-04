package li.strolch.model.query.parser;

import static org.petitparser.parser.primitive.CharacterParser.whitespace;
import static org.petitparser.parser.primitive.CharacterParser.word;
import static org.petitparser.parser.primitive.StringParser.ofIgnoringCase;

import org.petitparser.context.Result;
import org.petitparser.parser.Parser;
import org.petitparser.tools.CompositeParser;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StrolchElementQuery;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.model.visitor.NoStrategyActivityVisitor;
import li.strolch.model.visitor.NoStrategyOrderVisitor;
import li.strolch.model.visitor.NoStrategyResourceVisitor;
import li.strolch.utils.StringMatchMode;

public class QueryParser extends CompositeParser {

	private StrolchElementQuery<?> query;
	private OrSelection or;

	private IdSelection idSelection;

	/**
	 * Use static helper methods instead of constructors
	 * 
	 * @param resourceQuery
	 */
	private QueryParser(StrolchElementQuery<?> query) {
		// don't allow public construction
		this.query = query;
	}

	private OrSelection or() {
		if (this.or == null)
			this.or = query.or();
		return or;
	}

	@Override
	protected void initialize() {

		// [id:<value>] [name:<value>] [type:<value>] [param:<bagId>:<paramId>] [value]

		Parser id = ofIgnoringCase("id:").seq(word().star().flatten()).pick(1);
		Parser name = ofIgnoringCase("name:").seq(word().star().flatten()).pick(1);
		Parser type = ofIgnoringCase("type:").seq(word().star().flatten()).pick(1);

		def("id", id);
		def("name", name);
		def("type", type);

		Parser query = whitespace().optional().seq(ref("type").or(ref("id")).or(ref("name")).or(whitespace())).star();

		def("query", query);

		def("start", ref("query"));

		action("id", (String s) -> {
			String trimmed = s.trim();
			if (trimmed.isEmpty())
				return null;

			if (this.idSelection == null) {
				this.idSelection = new IdSelection(trimmed, StringMatchMode.ci());
				or().with(this.idSelection);
			} else {
				this.idSelection.with(trimmed);
			}

			return null;
		});
		action("name", (String s) -> {
			String trimmed = s.trim();
			if (!trimmed.isEmpty())
				or().with(new NameSelection(trimmed, StringMatchMode.ci()));
			return null;
		});
		action("type", (String s) -> {
			String trimmed = s.trim();
			if (!trimmed.isEmpty())
				this.query.setNavigation(new StrolchTypeNavigation(trimmed));
			return null;
		});

		action("start", o -> this.query);
	}

	public static ResourceQuery<Resource> parseToResourceQuery(String queryString, boolean withAny) {
		QueryParser parser = new QueryParser(new ResourceQuery<>());
		Result result = parser.parse(queryString);
		ResourceQuery<Resource> query = result.get();
		query.setResourceVisitor(new NoStrategyResourceVisitor());

		if (!query.hasSelection() && withAny) {
			query.withAny();
		}

		return query;
	}

	public static OrderQuery<Order> parseToOrderQuery(String queryString, boolean withAny) {
		QueryParser parser = new QueryParser(new OrderQuery<>());
		Result result = parser.parse(queryString);
		OrderQuery<Order> query = result.get();
		query.setOrderVisitor(new NoStrategyOrderVisitor());

		if (!query.hasSelection() && withAny) {
			query.withAny();
		}

		return query;
	}

	public static ActivityQuery<Activity> parseToActivityQuery(String queryString, boolean withAny) {
		QueryParser parser = new QueryParser(new ActivityQuery<>());
		Result result = parser.parse(queryString);
		ActivityQuery<Activity> query = result.get();
		query.setActivityVisitor(new NoStrategyActivityVisitor());

		if (!query.hasSelection() && withAny) {
			query.withAny();
		}

		return query;
	}
}
