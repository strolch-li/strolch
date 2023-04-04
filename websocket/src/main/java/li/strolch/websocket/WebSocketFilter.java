package li.strolch.websocket;

import static li.strolch.rest.filters.AuthenticationRequestFilter.getRemoteIp;

import jakarta.servlet.annotation.WebFilter;

import jakarta.servlet.*;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@WebFilter("/websocket/*")
public class WebSocketFilter implements Filter {

	private static final Logger logger = LoggerFactory.getLogger(WebSocketFilter.class);

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {

		HttpServletRequest httpRequest = (HttpServletRequest) request;
		String remoteIp = getRemoteIp(httpRequest);
		logger.info("Remote IP: " + remoteIp + ": " + httpRequest.getMethod() + " " + httpRequest.getRequestURI());
		WebSocketRemoteIp.set(remoteIp);
		chain.doFilter(request, response);
	}

	public void init(FilterConfig config) {
	}

	public void destroy() {
	}
}