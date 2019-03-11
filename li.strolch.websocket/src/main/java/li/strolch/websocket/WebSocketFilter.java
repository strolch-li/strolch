package li.strolch.websocket;

import static li.strolch.rest.filters.AuthenticationRequestFilter.getRemoteIp;

import javax.servlet.*;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
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
		logger.info("Remote IP: " + getRemoteIp(httpRequest) + ": " + httpRequest.getMethod() + " " + httpRequest
				.getRequestURI());
		chain.doFilter(request, response);
	}

	public void init(FilterConfig config) throws ServletException {
	}

	public void destroy() {
	}
}