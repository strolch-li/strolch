package li.strolch.soql.core.expresssion;

import java.util.Map;

/**
 * 
 * @author msmock
 *
 */
public interface IBooleanExpression extends IExpression {

	boolean evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter);

}
