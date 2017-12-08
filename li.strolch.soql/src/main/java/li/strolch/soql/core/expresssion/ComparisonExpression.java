package li.strolch.soql.core.expresssion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import li.strolch.soql.core.SOQLEvaluationException;

/**
 * Compares two or more expressions, for example 'a.name = "12345"'
 * <p>
 * Currently only the following operators for equality check are supported:
 * <p>
 * '=' | '<>' | '>' | '>=' | '<' | '<=' ;
 *
 * @author msmock
 */
public class ComparisonExpression extends AbstractBooleanExpression {

    private String operator;
    private List<IObjectExpression> operands = new ArrayList<>();

    @Override
    public boolean evaluate(Map<String, Object> inputObjects, Map<String, Object> queryParameter) {

        boolean result = false;

        switch (operator) {
            case "=":
                result = evaluateEquals(inputObjects, queryParameter);
                break;
            case "<>":
                result = !evaluateEquals(inputObjects, queryParameter);
                break;
            case ">":
                result = evaluateMore(inputObjects, queryParameter);
                break;
            case "<":
                result = evaluateLess(inputObjects, queryParameter);
                break;
            case ">=":
                result = !evaluateLess(inputObjects, queryParameter);
                break;
            case "<=":
                result = !evaluateMore(inputObjects, queryParameter);
                break;
            default:
                throw new SOQLEvaluationException("Comparison with operator " + operator + " is not supported yet.");
        }

        return result;
    }

    /**
     * TODO: allow comparison, if the classes do not match. I.e. compare Integer with Double
     *
     * @param inputObjects
     * @param queryParameter
     * @return
     */
    private boolean evaluateLess(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {

        final Object result_1 = operands.get(0).evaluate(inputObjects, queryParameter);
        final Object result_2 = operands.get(1).evaluate(inputObjects, queryParameter);

        final Class<?> clazz_1 = result_1.getClass();
        final Class<?> clazz_2 = result_2.getClass();

        if (!clazz_1.equals(clazz_2)) {
            throw new SOQLEvaluationException("Operation < not defined for comparison of " + result_1 + " of class "
                    + clazz_1 + " with " + result_2 + " of class " + clazz_2);
        }

        if (result_1 instanceof Integer && result_2 instanceof Integer) {
            return ((Integer) result_1).compareTo((Integer) result_2) < 0;
        } else if (result_1 instanceof Long && result_2 instanceof Long) {
            return ((Long) result_1).compareTo((Long) result_2) < 0;
        } else if (result_1 instanceof Float && result_2 instanceof Float) {
            return ((Float) result_1).compareTo((Float) result_2) < 0;
        } else if (result_1 instanceof Double && result_2 instanceof Double) {
            return ((Double) result_1).compareTo((Double) result_2) < 0;
        }

        return false;
    }

    private boolean evaluateMore(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {

        final Object result_1 = operands.get(0).evaluate(inputObjects, queryParameter);
        final Object result_2 = operands.get(1).evaluate(inputObjects, queryParameter);

        final Class<?> clazz_1 = result_1.getClass();
        final Class<?> clazz_2 = result_2.getClass();

        if (!clazz_1.equals(clazz_2)) {
            throw new SOQLEvaluationException("Operation < not defined for comparison of " + result_1 + " of class "
                    + clazz_1 + " with " + result_2 + " of class " + clazz_2);
        }

        if (result_1 instanceof Integer && result_2 instanceof Integer) {
            return ((Integer) result_1).compareTo((Integer) result_2) > 0;
        } else if (result_1 instanceof Long && result_2 instanceof Long) {
            return ((Long) result_1).compareTo((Long) result_2) > 0;
        } else if (result_1 instanceof Float && result_2 instanceof Float) {
            return ((Float) result_1).compareTo((Float) result_2) > 0;
        } else if (result_1 instanceof Double && result_2 instanceof Double) {
            return ((Double) result_1).compareTo((Double) result_2) > 0;
        }

        return false;
    }

    /**
     * @return true if the String representation of the operands match
     */
    private boolean evaluateEquals(final Map<String, Object> inputObjects, final Map<String, Object> queryParameter) {
        Object result_1 = operands.get(0).evaluate(inputObjects, queryParameter);
        Object result_2 = operands.get(1).evaluate(inputObjects, queryParameter);
        if (result_1 == null || result_2 == null)
            return false;
        return result_1.equals(result_2);
    }

    public void setOperator(String operator) {
        this.operator = operator;
    }

    public void addOperand(IObjectExpression operand) {
        operands.add(operand);
        operand.setParent(this);
    }

    @Override
    public String toString() {
        return "ComparisonExpression [operator=" + operator + ", operands=" + operands + "]";
    }

}
