package li.strolch.search;

import static li.strolch.utils.helper.ExceptionHelper.getExceptionMessage;

import java.util.Collection;
import java.util.stream.Stream;

import li.strolch.exception.StrolchAccessDeniedException;
import li.strolch.model.StrolchModelConstants;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Restrictable;

public class StrolchValueSearch<T> extends ValueSearch<T> implements Restrictable {

	private String privilegeValue;

	public StrolchValueSearch() {
		this.privilegeValue = getClass().getName();
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

	/**
	 * Marks this search as an internal search, thus allowing it to be performed without the authenticated user having
	 * the specific privilege, provided the {@link StrolchModelConstants#INTERNAL} flag is on the search privilege
	 *
	 * @return this object for chaining
	 */
	public StrolchValueSearch<T> internal() {
		this.privilegeValue = StrolchModelConstants.INTERNAL;
		return this;
	}

	protected void assertHasPrivilege(PrivilegeContext ctx) {
		try {
			ctx.validateAction(this);
		} catch (PrivilegeModelException e) {
			throw e;
		} catch (PrivilegeException e) {
			throw new StrolchAccessDeniedException(ctx.getCertificate(), this, getExceptionMessage(e), e);
		}
	}

	@Override
	public SearchResult<T> search(Stream<T> stream) {
		return super.search(stream);
	}

	@Override
	public SearchResult<T> search(Collection<T> input) {
		return super.search(input);
	}
}
