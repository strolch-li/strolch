package li.strolch.rest.util;

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
	private static final long serialVersionUID = 1L;
	public String baseUrl;
	public String objectId;
	public JsonElement jsonElement;
}
