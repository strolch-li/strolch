package li.strolch.search;

import java.util.stream.Stream;

import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.ExceptionHelper;

public abstract class StrolchSearch implements Restrictable {

	private String privilegeValue;

	private SearchNavigator navigator;
	private SearchExpression expression;

	public StrolchSearch() {
		this.privilegeValue = getClass().getName();
	}

	/**
	 * Marks this search as an internal search, thus allowing it to be performed without the authenticated user to need
	 * the required privilege
	 *
	 * @return this object for chaining
	 */
	public StrolchSearch internal() {
		this.privilegeValue = StrolchModelConstants.INTERNAL;
		return this;
	}

	public final RootElementSearchResult<StrolchRootElement> search(StrolchTransaction tx) {
		try {
			PrivilegeContext privilegeContext = tx.getContainer().getPrivilegeHandler().validate(tx.getCertificate());
			privilegeContext.validateAction(this);
		} catch (PrivilegeException e) {
			throw new StrolchAccessDeniedException(tx.getCertificate(), this, ExceptionHelper.getExceptionMessage(e),
					e);
		}

		// first prepare
		prepare();

		// then validate status
		DBC.PRE.assertNotNull("navigation not set! Call one of resources(), orders() or activities()", this.navigator);
		DBC.PRE.assertNotNull("expression not set! Call where()!", this.expression);

		@SuppressWarnings("unchecked")
		Stream<StrolchRootElement> stream = (Stream<StrolchRootElement>) this.navigator.navigate(tx)
				.filter(e -> this.expression.matches(e));
		return new RootElementSearchResult<StrolchRootElement>(stream);
	}

	protected abstract StrolchSearch prepare();

	// Navigator

	protected final StrolchSearch resources(String... types) {
		this.navigator = tx -> tx.streamResources(types);
		return this;
	}

	protected final StrolchSearch orders(String... types) {
		this.navigator = tx -> tx.streamOrders(types);
		return this;
	}

	protected final StrolchSearch activities(String... types) {
		this.navigator = tx -> tx.streamActivities(types);
		return this;
	}

	//
	// Expressions
	//

	protected final StrolchSearch where(SearchExpression searchExpression) {
		if (this.expression == null)
			this.expression = searchExpression;
		else
			this.expression = this.expression.and(searchExpression);
		return this;
	}

	protected final SearchExpression id(SearchPredicate predicate) {
		return SearchExpressions.id(predicate);
	}

	protected final SearchExpression name(SearchPredicate predicate) {
		return SearchExpressions.name(predicate);
	}

	protected final SearchExpression date(SearchPredicate predicate) {
		return SearchExpressions.date(predicate);
	}

	protected final SearchExpression state(SearchPredicate predicate) {
		return SearchExpressions.state(predicate);
	}

	protected final SearchExpression param(String bagId, String paramId, SearchPredicate predicate) {
		return SearchExpressions.param(bagId, paramId, predicate);
	}

	protected final SearchExpression paramNull(String bag, String param) {
		return SearchExpressions.paramNull(bag, param);
	}

	protected final SearchExpression not(SearchExpression expression) {
		return element -> !expression.matches(element);
	}

	//
	// Predicates
	//

	protected final SearchPredicate isEqualTo(Object value) {
		return SearchPredicates.isEqualTo(value, false);
	}

	protected final SearchPredicate isEqualToIgnoreCase(Object value) {
		return SearchPredicates.isEqualTo(value, true);
	}

	protected final SearchPredicate isNotEqualTo(Object value) {
		return SearchPredicates.isEqualTo(value, false).not();
	}

	protected final SearchPredicate isNotEqualToIgnoreCase(Object value) {
		return SearchPredicates.isEqualTo(value, true).not();
	}

	protected final SearchPredicate startsWith(Object value) {
		return SearchPredicates.startsWith(value, false);
	}

	protected final SearchPredicate startsWithIgnoreCase(Object value) {
		return SearchPredicates.startsWith(value, true);
	}

	protected final SearchPredicate endsWith(Object value) {
		return SearchPredicates.endsWith(value, false);
	}

	protected final SearchPredicate endsWithIgnoreCase(Object value) {
		return SearchPredicates.endsWith(value, true);
	}

	protected final SearchPredicate contains(Object value) {
		return SearchPredicates.contains(value, false);
	}

	protected final SearchPredicate containsIgnoreCase(Object value) {
		return SearchPredicates.contains(value, true);
	}

	protected final SearchPredicate inRange(DateRange range) {
		return SearchPredicates.inRange(range);
	}

	/**
	 * @see li.strolch.privilege.model.Restrictable#getPrivilegeName()
	 */
	@Override
	public String getPrivilegeName() {
		return StrolchSearch.class.getName();
	}

	/**
	 * @see li.strolch.privilege.model.Restrictable#getPrivilegeValue()
	 */
	@Override
	public Object getPrivilegeValue() {
		return this.privilegeValue;
	}
}
