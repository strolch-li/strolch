package li.strolch.model;

import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static li.strolch.model.StrolchModelConstants.BAG_RELATIONS;

import li.strolch.model.parameter.StringParameter;

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
}
