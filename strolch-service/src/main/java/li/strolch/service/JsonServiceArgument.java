package li.strolch.service;

import com.google.gson.JsonElement;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;

/**
 * A {@link ServiceArgument} which takes a {@link JsonElement} as the input for a {@link AbstractService}. This is often
 * used in conjunction with REST APIs
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class JsonServiceArgument extends ServiceArgument {

	/**
	 * the objectType - its context is defined by the service
	 */
	public String objectType;

	/**
	 * the objectId - its context is defined by the service
	 */
	public String objectId;

	/**
	 * The input object
	 */
	public JsonElement jsonElement;

	public JsonServiceArgument() {
	}

	public JsonServiceArgument(JsonElement jsonElement) {
		this.jsonElement = jsonElement;
	}

	public JsonServiceArgument(String objectId, JsonElement jsonElement) {
		this.objectId = objectId;
		this.jsonElement = jsonElement;
	}

	public JsonServiceArgument(String objectType, String objectId, JsonElement jsonElement) {
		this.objectType = objectType;
		this.objectId = objectId;
		this.jsonElement = jsonElement;
	}
}
