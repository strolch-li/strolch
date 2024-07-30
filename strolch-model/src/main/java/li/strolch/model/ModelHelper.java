package li.strolch.model;

import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringParameter;

import static li.strolch.model.StrolchModelConstants.*;

/**
 * Helper class to perform often performed model commands
 */
public class ModelHelper {

	public static void copyExistingString(Resource src, Resource dst, String paramId) {
		copyStringParam(src, dst, BAG_PARAMETERS, paramId);
	}

	public static void copyExistingRelation(Resource src, Resource dst, String paramId) {
		copyStringParam(src, dst, BAG_RELATIONS, paramId);
	}

	public static void copyStringParam(Resource src, Resource dst, String bagId, String paramId) {
		if (src.hasParameter(bagId, paramId) && !src.getStringP(bagId, paramId).isEmpty()) {
			StringParameter existingBillingIdP = src.getStringP(bagId, paramId);
			if (!dst.hasParameter(bagId, paramId)) {
				dst.addParameter(bagId, existingBillingIdP.getClone());
			} else if (dst.getStringP(bagId, paramId).isEmpty()) {
				dst.setString(bagId, paramId, existingBillingIdP.getValue());
			}
		}
	}

	public static void setInterpretationAndUom(Parameter<?> relationP, String objectType, String elementType) {
		switch (objectType) {
			case Tags.RESOURCE -> relationP.setInterpretation(INTERPRETATION_RESOURCE_REF);
			case Tags.ORDER -> relationP.setInterpretation(INTERPRETATION_ORDER_REF);
			case Tags.ACTIVITY -> relationP.setInterpretation(INTERPRETATION_ACTIVITY_REF);
			default -> throw new IllegalStateException("Unexpected value: " + objectType);
		}

		relationP.setUom(elementType);
	}
}
