package li.strolch.model.query.parser;

import static org.petitparser.parser.primitive.CharacterParser.digit;
import static org.petitparser.parser.primitive.CharacterParser.letter;
import static org.petitparser.parser.primitive.CharacterParser.of;
import static org.petitparser.parser.primitive.CharacterParser.whitespace;
import static org.petitparser.parser.primitive.CharacterParser.word;
import static org.petitparser.parser.primitive.StringParser.ofIgnoringCase;

import java.util.Set;

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
import li.strolch.model.query.ParameterSelection;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.StrolchElementQuery;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.model.visitor.NoStrategyActivityVisitor;
import li.strolch.model.visitor.NoStrategyOrderVisitor;
import li.strolch.model.visitor.NoStrategyResourceVisitor;
import li.strolch.utils.StringMatchMode;
import li.strolch.utils.collections.MapOfSets;

public abstract class QueryParser extends CompositeParser {

	private StrolchElementQuery<?> query;
	private OrSelection or;

	private IdSelection idSelection;
	private boolean allowType;

	public QueryParser(StrolchElementQuery<?> query) {
		this(query, false);
	}

	public QueryParser(StrolchElementQuery<?> query, boolean allowType) {
		this.query = query;
		this.allowType = allowType;
	}

	protected OrSelection or() {
		if (this.or == null)
			this.or = query.or();
		return or;
	}

	private Parser[] charParsers() {
		return new Parser[] { of('@'), of('.'), of('-'), of('_'), of('+'), of(':') };
	}

	protected Parser key(String key) {
		return ofIgnoringCase(key + ":").seq(word().or(charParsers()).star().flatten()).pick(1);
	}

	public abstract MapOfSets<String, String> getBagParamSet();

	public abstract boolean withPrefix();

	protected void defs() {

		// [id:<value>] [name:<value>] [type:<value>] [param:<bagId>:<paramId>] [value]

		Parser parsers = null;

		if (withPrefix()) {
			def("id", key("id"));
			def("name", key("name"));
			if (this.allowType)
				def("type", key("type"));

			for (String bagId : getBagParamSet().keySet()) {
				Set<String> set = getBagParamSet().getSet(bagId);
				for (String paramId : set) {

					def(paramId, key(paramId));

					if (parsers == null) {
						parsers = ref(paramId);
					} else {
						parsers = parsers.or(ref(paramId));
					}
				}
			}

			if (parsers == null)
				parsers = ref("id").or(ref("name"));
			else
				parsers = parsers.or(ref("id")).or(ref("name"));

			if (this.allowType)
				parsers = parsers.or(ref("type"));

		} else {
			def("other", letter().or(digit()).seq(word().or(charParsers()).star().flatten()).flatten());
			parsers = ref("other");
		}

		parsers = parsers.or(whitespace());

		Parser query = whitespace().optional().seq(parsers).star();

		def("query", query);

		def("start", ref("query"));
	}

	protected void actions() {

		if (withPrefix()) {
			for (String bagId : getBagParamSet().keySet()) {
				Set<String> set = getBagParamSet().getSet(bagId);
				for (String paramId : set) {
					action(paramId, (String s) -> {
						String trimmed = s.trim();
						if (!trimmed.isEmpty())
							or().with(
									ParameterSelection.anyTypeSelection(bagId, paramId, trimmed, StringMatchMode.ci()));
						return null;
					});
				}
			}

			action("id", (String s) -> {
				doIdAction(s);
				return null;
			});
			action("name", (String s) -> {
				doNameAction(s);
				return null;
			});

			if (this.allowType) {
				action("type", (String s) -> {
					String trimmed = s.trim();
					if (!trimmed.isEmpty())
						this.query.setNavigation(new StrolchTypeNavigation(trimmed));
					return null;
				});
			}

		} else {

			action("other", (String s) -> {
				doIdAction(s);
				doNameAction(s);
				doParamAction(s);
				return null;
			});
		}

		action("start", o -> this.query);
	}

	private void doIdAction(String s) {
		String trimmed = s.trim();
		if (trimmed.isEmpty())
			return;

		if (this.idSelection == null) {
			this.idSelection = new IdSelection(trimmed, StringMatchMode.ci());
			or().with(this.idSelection);
		} else {
			this.idSelection.with(trimmed);
		}
	}

	private void doNameAction(String s) {
		String trimmed = s.trim();
		if (!trimmed.isEmpty())
			or().with(new NameSelection(trimmed, StringMatchMode.ci()));
	}

	private void doParamAction(String s) {
		for (String bagId : getBagParamSet().keySet()) {
			Set<String> set = getBagParamSet().getSet(bagId);
			for (String paramId : set) {
				String trimmed = s.trim();
				if (!trimmed.isEmpty())
					or().with(ParameterSelection.anyTypeSelection(bagId, paramId, trimmed, StringMatchMode.ci()));
			}
		}
	}

	@Override
	protected void initialize() {
		defs();
		actions();
	}

	public static ResourceQuery<Resource> parseToResourceQuery(String queryString, boolean withPrefix,
			boolean withAny) {
		return parseToResourceQuery(new MapOfSets<>(), withPrefix, queryString, withAny);
	}

	public static ResourceQuery<Resource> parseToResourceQuery(MapOfSets<String, String> bagParamSet,
			boolean withPrefix, String queryString, boolean withAny) {
		QueryParser parser = new QueryParser(new ResourceQuery<>()) {
			@Override
			public MapOfSets<String, String> getBagParamSet() {
				return bagParamSet;
			}

			@Override
			public boolean withPrefix() {
				return withPrefix;
			}
		};
		Result result = parser.parse(queryString);
		ResourceQuery<Resource> query = result.get();
		query.setResourceVisitor(new NoStrategyResourceVisitor());

		if (!query.hasSelection() && withAny) {
			query.withAny();
		}

		return query;
	}

	public static OrderQuery<Order> parseToOrderQuery(String queryString, boolean withPrefix, boolean withAny) {
		return parseToOrderQuery(new MapOfSets<>(), withPrefix, queryString, withAny);
	}

	public static OrderQuery<Order> parseToOrderQuery(MapOfSets<String, String> bagParamSet, boolean withPrefix,
			String queryString, boolean withAny) {
		QueryParser parser = new QueryParser(new OrderQuery<>()) {
			@Override
			public MapOfSets<String, String> getBagParamSet() {
				return bagParamSet;
			}

			@Override
			public boolean withPrefix() {
				return withPrefix;
			}
		};
		Result result = parser.parse(queryString);
		OrderQuery<Order> query = result.get();
		query.setOrderVisitor(new NoStrategyOrderVisitor());

		if (!query.hasSelection() && withAny) {
			query.withAny();
		}

		return query;
	}

	public static ActivityQuery<Activity> parseToActivityQuery(String queryString, boolean withPrefix,
			boolean withAny) {
		return parseToActivityQuery(new MapOfSets<>(), withPrefix, queryString, withAny);
	}

	public static ActivityQuery<Activity> parseToActivityQuery(MapOfSets<String, String> bagParamSet,
			boolean withPrefix, String queryString, boolean withAny) {
		QueryParser parser = new QueryParser(new ActivityQuery<>()) {
			@Override
			public MapOfSets<String, String> getBagParamSet() {
				return bagParamSet;
			}

			@Override
			public boolean withPrefix() {
				return withPrefix;
			}
		};
		Result result = parser.parse(queryString);
		ActivityQuery<Activity> query = result.get();
		query.setActivityVisitor(new NoStrategyActivityVisitor());

		if (!query.hasSelection() && withAny) {
			query.withAny();
		}

		return query;
	}
}
