package li.strolch.rest.filters;

import java.io.IOException;

import javax.annotation.Priority;
import javax.ws.rs.Priorities;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.Provider;

@Provider
@Priority(Priorities.HEADER_DECORATOR)
public class HttpCacheResponseFilter implements ContainerResponseFilter {

	public static final String NO_CACHE = "no-cache"; //$NON-NLS-1$

	private static String cacheMode = NO_CACHE;
	
	@Override
	public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext)
			throws IOException {

		MultivaluedMap<String, Object> headers = responseContext.getHeaders();
		headers.add(HttpHeaders.CACHE_CONTROL, cacheMode);
	}

	/**
	 * @return the cacheMode
	 */
	public static String getCacheMode() {
		return cacheMode;
	}

	/**
	 * @param cacheMode
	 *            the cacheMode to set
	 */
	public static void setCacheMode(String cacheMode) {
		HttpCacheResponseFilter.cacheMode = cacheMode;
	}
}