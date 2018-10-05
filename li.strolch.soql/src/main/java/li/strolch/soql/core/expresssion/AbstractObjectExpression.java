/**
 *
 */
package li.strolch.soql.core.expresssion;

import java.util.Map;

/**
 * Abstract class for all expressions which evaluate to other objects than Boolean
 *
 * @author msmock
 */
public abstract class AbstractObjectExpression implements IObjectExpression {

	IExpression parent;

	@Override
	public abstract Class<?> getType(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter);

	@Override
	public abstract Object evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter);

	@Override
	public void setParent(IExpression e) {
		parent = e;
	}

	@Override
	public IExpression getParent() {
		return parent;
	}

}
