package li.strolch.soql.core.expresssion;

import java.util.Map;

public interface IObjectExpression extends IExpression {
	
	Object evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter); 
	
	Class<?> getType(Map<String, Object> inputObjects, Map<String, Object> queryParameter); 
	
}
