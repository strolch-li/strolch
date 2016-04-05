package li.strolch.model.query.parser;

import static org.petitparser.parser.primitive.CharacterParser.whitespace;
import static org.petitparser.parser.primitive.CharacterParser.word;
import static org.petitparser.parser.primitive.StringParser.ofIgnoringCase;

import org.petitparser.context.Result;
import org.petitparser.parser.Parser;
import org.petitparser.tools.CompositeParser;

import ch.eitchnet.utils.StringMatchMode;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StrolchElementQuery;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.model.visitor.NoStrategyOrderVisitor;
import li.strolch.model.visitor.NoStrategyResourceVisitor;

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
			if (this.idSelection == null) {
				this.idSelection = new IdSelection(s.trim(), StringMatchMode.ci());
				or().with(this.idSelection);
			} else {
				this.idSelection.with(s.trim());
			}
			return null;
		});
		action("name", (String s) -> {
			or().with(new NameSelection(s.trim(), StringMatchMode.ci()));
			return null;
		});
		action("type", (String s) -> {
			this.query.setNavigation(new StrolchTypeNavigation(s.trim()));
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
}
