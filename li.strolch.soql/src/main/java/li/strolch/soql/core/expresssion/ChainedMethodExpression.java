package li.strolch.soql.core.expresssion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * extract state field value by chained method call
 * 
 * @author msmock
 */
public class ChainedMethodExpression extends AbstractObjectExpression {

	private String objectKey;

	private List<MethodExpression> methodExpressions = new ArrayList<>();

	@Override
	public Class<?> getType(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		Object result = evaluate(inputObjects, queryParameter);
		return result.getClass();
	}

	/**
	 * evaluate by calling the method by name.
	 */
	@Override
	public Object evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		
		final Object inputObject = inputObjects.get(objectKey);
		
		Object object = inputObject; 
		for (MethodExpression methodExpression : methodExpressions) {			
			methodExpression.setObject(object);
			object = methodExpression.evaluate(inputObjects, queryParameter);
		}
		
		return object;
	}

	public void setObjectKey(String entityKey) {
		this.objectKey = entityKey;
	}

	public void addMethodExpression(MethodExpression methodExpression) {
		this.methodExpressions.add(methodExpression);
		methodExpression.setParent(this);
	}

	@Override
	public String toString() {
		return "ChainedMethodExpression [objectKey=" + objectKey + ", methodExpressions=" + methodExpressions + "]";
	}

}
