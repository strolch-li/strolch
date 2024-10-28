package li.strolch.soql.core.expresssion;

public interface IExpression {

	void setParent(IExpression e);

	IExpression getParent();

}
