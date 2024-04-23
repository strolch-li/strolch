package li.strolch.rest.helper;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import li.strolch.rest.RestfulStrolchComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Enumeration;

public class ServletRequestHelper {

	private static final Logger logger = LoggerFactory.getLogger(ServletRequestHelper.class);

	public static void logRequest(HttpServletRequest request) {
		if (!RestfulStrolchComponent.getInstance().isRestLogging())
			return;

		try {
			StringBuilder sb = new StringBuilder();
			sb.append("REQUEST: ").append(request.getMethod()).append(" ").append(request.getRequestURI()).append("\n");
			sb
					.append("From: ")
					.append(request.getRemoteAddr())
					.append("/")
					.append(request.getRemoteHost())
					.append(" ")
					.append(request.getRemotePort())
					.append("\n");

			sb.append("AuthType: ").append(request.getAuthType()).append("\n");

			Cookie[] cookies = request.getCookies();
			if (cookies == null) {
				sb.append("Cookies: NONE!\n");
			} else {
				sb.append("Cookies: \n");
				for (Cookie cookie : cookies) {
					sb.append("  ").append(cookie.getName()).append(" = ").append(cookie.getValue()).append("\n");
				}
			}

			Enumeration<String> headerNames = request.getHeaderNames();
			if (!headerNames.hasMoreElements()) {
				sb.append("Headers: NONE!\n");
			} else {
				sb.append("Headers: \n");
				while (headerNames.hasMoreElements()) {
					String headerName = headerNames.nextElement();
					sb.append("  ").append(headerName).append(" = ").append(request.getHeader(headerName)).append("\n");
				}
			}

			logger.info(sb.toString());
		} catch (Exception e) {
			logger.error("Failed to log request", e);
		}
	}
}
