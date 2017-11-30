/**
 * 
 */
package li.strolch.soql.core.expresssion;

import java.util.Map;

/**
 * 
 * Abstract class for all expressions which evaluate to Boolean result
 * 
 * @author msmock
 *
 */
public abstract class AbstractBooleanExpression implements IBooleanExpression {
	
	IExpression parent;

	@Override
	public abstract boolean evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter); 

	@Override
	public void setParent(IExpression e) {
		parent = e; 
	}

	@Override
	public IExpression getParent() {
		return parent;
	}

}
