package li.strolch.model.activity;

public interface TimeOrderingVisitor {

	public void visitSeries(Activity activity);

	public void visitParallel(Activity activity);
}
