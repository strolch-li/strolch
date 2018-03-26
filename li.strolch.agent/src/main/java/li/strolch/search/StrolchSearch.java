package li.strolch.search;

import java.util.stream.Stream;

import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.ExceptionHelper;

public abstract class StrolchSearch<T extends StrolchRootElement>
		implements SearchExpressions, SearchPredicates, Restrictable {

	private String privilegeValue;

	private SearchNavigator<T> navigator;
	private SearchExpression expression;

	public StrolchSearch() {
		this.privilegeValue = getClass().getName();
	}

	/**
	 * Allows to implement the search expression in one method, or to prepare a project specific search expression
	 */
	protected abstract void define();

	/**
	 * Adds  the given {@link SearchExpression} to the current search
	 *
	 * @param expression
	 * 		the {@link SearchExpression} to add to this search
	 *
	 * @return this for chaining
	 */
	public StrolchSearch<T> where(SearchExpression expression) {
		if (this.expression == null)
			this.expression = expression;
		else
			this.expression = this.expression.and(expression);
		return this;
	}

	/**
	 * Marks this search as an internal search, thus allowing it to be performed without the authenticated user to need
	 * the required privilege
	 *
	 * @return this object for chaining
	 */
	public StrolchSearch<T> internal() {
		this.privilegeValue = StrolchModelConstants.INTERNAL;
		return this;
	}

	public RootElementSearchResult<T> search(StrolchTransaction tx) {
		try {
			PrivilegeContext privilegeContext = tx.getContainer().getPrivilegeHandler().validate(tx.getCertificate());
			privilegeContext.validateAction(this);
		} catch (PrivilegeException e) {
			throw new StrolchAccessDeniedException(tx.getCertificate(), this, ExceptionHelper.getExceptionMessage(e),
					e);
		}

		// first prepare
		define();

		// then validate status
		DBC.PRE.assertNotNull("navigation not set! Call one of resources(), orders() or activities()", this.navigator);

		Stream<T> stream = this.navigator.navigate(tx);

		if (this.expression != null)
			stream = stream.filter(e -> this.expression.matches(e));

		return new RootElementSearchResult<T>(stream);
	}

	@SuppressWarnings("unchecked")
	protected StrolchSearch<T> resources(String... types) {
		this.navigator = tx -> (Stream<T>) tx.streamResources(types);
		return this;
	}

	@SuppressWarnings("unchecked")
	protected StrolchSearch<T> orders(String... types) {
		this.navigator = tx -> (Stream<T>) tx.streamOrders(types);
		return this;
	}

	@SuppressWarnings("unchecked")
	protected StrolchSearch<T> activities(String... types) {
		this.navigator = tx -> (Stream<T>) tx.streamActivities(types);
		return this;
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
