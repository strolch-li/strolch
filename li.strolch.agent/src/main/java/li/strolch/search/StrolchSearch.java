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
	 * Marks this search as an internal search, thus allowing it to be performed without the authenticated user to need
	 * the required privilege
	 *
	 * @return this object for chaining
	 */
	public StrolchSearch internal() {
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
		DBC.PRE.assertNotNull("expression not set! Call where()!", this.expression);

		Stream<T> stream = this.navigator.navigate(tx).filter(e -> this.expression.matches(e));
		return new RootElementSearchResult<T>(stream);
	}

	protected abstract void define();

	@SuppressWarnings("unchecked")
	protected StrolchSearch resources(String... types) {
		this.navigator = tx -> (Stream<T>) tx.streamResources(types);
		return this;
	}

	@SuppressWarnings("unchecked")
	protected StrolchSearch orders(String... types) {
		this.navigator = tx -> (Stream<T>) tx.streamOrders(types);
		return this;
	}

	@SuppressWarnings("unchecked")
	protected StrolchSearch activities(String... types) {
		this.navigator = tx -> (Stream<T>) tx.streamActivities(types);
		return this;
	}

	protected StrolchSearch where(SearchExpression searchExpression) {
		if (this.expression == null)
			this.expression = searchExpression;
		else
			this.expression = this.expression.and(searchExpression);
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
