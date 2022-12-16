package li.strolch.soql.core.expresssion;

import java.util.Map;

/**
 * @author msmock
 */
public class ObjectDeclaration extends AbstractObjectExpression {

	public String key;

	@Override
	public Object evaluate(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
		return inputObjects.get(key);
	}

	@Override
	public Class<?> getType(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		return inputObjects.get(key).getClass();
	}

	@Override
	public String toString() {
		return "ObjectDeclaration [key=" + key + "]";
	}
}
