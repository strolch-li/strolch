package li.strolch.rest.filters;

import java.io.IOException;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.ext.Provider;

/**
 * The JSON generated is not in the same charset as the rest of the response, thus we override it to UTF-8 with this
 * response filter
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@Provider
public class CharsetResponseFilter implements ContainerResponseFilter {

	private static final String UTF_8 = "utf-8"; //$NON-NLS-1$

	@Override
	public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext)
			throws IOException {

		MediaType contentType = responseContext.getMediaType();
		if (contentType != null) {
			String charset = contentType.getParameters().get(MediaType.CHARSET_PARAMETER);
			if (charset == null || !charset.equalsIgnoreCase(UTF_8)) {
				contentType = contentType.withCharset(UTF_8);
				responseContext.getHeaders().putSingle(HttpHeaders.CONTENT_TYPE, contentType.toString());
			}
		}
	}
}