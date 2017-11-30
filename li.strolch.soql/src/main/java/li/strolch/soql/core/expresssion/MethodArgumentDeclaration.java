package li.strolch.soql.core.expresssion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Expression to wrap a declaration of method arguments
 * 
 * @author msmock
 *
 */
public class MethodArgumentDeclaration extends AbstractObjectExpression {

	private List<ParameterReference> parameterReferences = new ArrayList<>();

	@Override
	public Class<?> getType(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		return List.class;
	}

	@Override
	public Object evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {
		final List<Object> result = new ArrayList<>(); 
		for (ParameterReference parameterReference : parameterReferences) {
			result.add(parameterReference.evaluate(inputObjects, queryParameter)); 
		}
		return result;
	}

	public void addParameterReference(final ParameterReference parameterReference) {
		parameterReferences.add(parameterReference);
		parameterReference.setParent(this);
	}

	@Override
	public String toString() {
		return "MethodArguments [parameterReferences=" + parameterReferences + "]";
	}

}
