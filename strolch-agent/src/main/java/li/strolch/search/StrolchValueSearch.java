package li.strolch.search;

import li.strolch.model.StrolchModelConstants;
import li.strolch.privilege.model.Restrictable;

import java.util.Collection;
import java.util.stream.Stream;

import static li.strolch.model.StrolchModelConstants.INTERNAL;

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
		this.privilegeValue = INTERNAL;
		return this;
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
