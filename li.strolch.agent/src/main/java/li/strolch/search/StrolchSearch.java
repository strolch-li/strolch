package li.strolch.search;

import static li.strolch.model.StrolchModelConstants.INTERNAL;
import static li.strolch.model.Tags.AGENT;

import java.util.Collection;
import java.util.ResourceBundle;
import java.util.stream.Stream;

import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.model.Restrictable;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.I18nMessage;
import li.strolch.utils.dbc.DBC;

/**
 * Class to perform searches on Strolch elements
 *
 * @param <T>
 */
public abstract class StrolchSearch<T extends StrolchRootElement>
		implements SearchExpressions, SearchPredicates, Restrictable {

	private String privilegeValue;

	private SearchExpression<T> expression;

	public StrolchSearch() {
		this.privilegeValue = getClass().getName();
	}

	protected abstract SearchNavigator<T> getNavigator();

	/**
	 * Used to configure the navigator, i.e. which <code>type</code> of root elements are to be queried
	 *
	 * @param types
	 * 		the types of elements to search
	 *
	 * @return this for chaining
	 */
	public abstract StrolchSearch<T> types(String... types);

	/**
	 * Allows to implement the search expression in one method, or to prepare a project specific search expression
	 */
	protected void define() {
		// default is no-op
	}

	/**
	 * Adds  the given {@link SearchExpression} to the current search
	 *
	 * @param expression
	 * 		the {@link SearchExpression} to add to this search
	 *
	 * @return this for chaining
	 */
	public StrolchSearch<T> where(SearchExpression<T> expression) {
		if (this.expression == null)
			this.expression = expression;
		else
			this.expression = this.expression.and(expression);
		return this;
	}

	/**
	 * Marks this search as an internal search, thus allowing it to be performed without the authenticated user having
	 * the specific privilege, provided the {@link StrolchModelConstants#INTERNAL} flag is on the search privilege
	 *
	 * @return this object for chaining
	 */
	public StrolchSearch<T> internal() {
		this.privilegeValue = INTERNAL;
		return this;
	}

	/**
	 * Performs the actual search, by first validating the privilege context
	 *
	 * @param tx
	 * 		the TX on which to perform the search
	 *
	 * @return the search result
	 */
	public RootElementSearchResult<T> search(StrolchTransaction tx) {
		try {
			tx.getPrivilegeContext().validateAction(this);
		} catch (AccessDeniedException e) {

			String username = tx.getCertificate().getUsername();

			if (tx.getContainer().hasComponent(OperationsLog.class)) {
				String realmName = tx.getRealmName();
				String searchName = this.privilegeValue.equals(INTERNAL) ?
						(getClass().getName() + " (INTERNAL)") :
						this.privilegeValue;
				LogMessage logMessage = new LogMessage(realmName, username,
						Locator.valueOf(AGENT, PrivilegeHandler.class.getSimpleName(), getPrivilegeName(), searchName),
						LogSeverity.Exception, LogMessageState.Information, ResourceBundle.getBundle("strolch-agent"),
						"agent.search.failed.access.denied").value("user", username).value("search", searchName)
						.withException(e);

				OperationsLog operationsLog = tx.getContainer().getComponent(OperationsLog.class);
				operationsLog.addMessage(logMessage);
			}

			String searchName = this.privilegeValue.equals(INTERNAL) ?
					(getClass().getSimpleName() + " (INTERNAL)") :
					getClass().getSimpleName();
			I18nMessage i18n = new I18nMessage(
					ResourceBundle.getBundle("strolch-agent", tx.getCertificate().getLocale()),
					"agent.search.failed.access.denied").value("user", username).value("search", searchName);

			throw new StrolchAccessDeniedException(tx.getCertificate(), this, i18n, e);
		}

		// first prepare
		define();

		// then validate navigator
		DBC.PRE.assertNotNull("navigation not set! Call types()", getNavigator());

		Stream<T> stream = getNavigator().navigate(tx);

		if (this.expression != null)
			stream = stream.filter(e -> this.expression.matches(e));

		return new RootElementSearchResult<>(stream);
	}

	/**
	 * Performs the actual search on the given input list
	 *
	 * @return the search result
	 */
	public RootElementSearchResult<T> search(Collection<T> input) {

		// first prepare
		define();

		Stream<T> stream = input.stream();

		if (this.expression != null)
			stream = stream.filter(e -> this.expression.matches(e));

		return new RootElementSearchResult<>(stream);
	}

	/**
	 * Performs the actual search on the given input stream
	 *
	 * @return the search result
	 */
	public RootElementSearchResult<T> search(Stream<T> input) {

		// first prepare
		define();

		Stream<T> stream = input;

		if (this.expression != null)
			stream = stream.filter(e -> this.expression.matches(e));

		return new RootElementSearchResult<>(stream);
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
