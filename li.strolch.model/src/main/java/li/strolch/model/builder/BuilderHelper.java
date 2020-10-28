package li.strolch.model.builder;

public class BuilderHelper {

	public static String buildParamId(String type) {
		return Character.toLowerCase(type.charAt(0)) + type.substring(1);
	}

	public static String buildParamName(String paramId) {
		return Character.toUpperCase(paramId.charAt(0)) + paramId.substring(1);
	}
}
