package li.strolch.model.activity;

public interface TimeOrderingVisitor {

	void visitSeries(Activity activity);

	void visitParallel(Activity activity);
}
