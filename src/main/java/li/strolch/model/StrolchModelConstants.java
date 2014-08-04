package li.strolch.model;

import li.strolch.model.parameter.Parameter;

public class StrolchModelConstants {

	/**
	 * The type to set on {@link StrolchRootElement StrolchRootElements} when defining a template for a type of element
	 */
	public static final String TEMPLATE = "Template"; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as a reference
	 * to a {@link Resource}
	 */
	public static final String INTERPRETATION_RESOURCE_REF = "Resource-Ref"; //$NON-NLS-1$

	/**
	 * This interpretation value indicates that the value of the {@link Parameter} should be understood as a reference
	 * to an {@link Order}
	 */
	public static final String INTERPRETATION_ORDER_REF = "Order-Ref"; //$NON-NLS-1$
}
