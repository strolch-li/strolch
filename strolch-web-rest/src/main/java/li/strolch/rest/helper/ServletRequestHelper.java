package li.strolch.rest.helper;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import li.strolch.rest.RestfulStrolchComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Enumeration;

import static li.strolch.utils.helper.StringHelper.*;

public class ServletRequestHelper {

	private static final Logger logger = LoggerFactory.getLogger(ServletRequestHelper.class);

	private static final int PADDING = 30;
	private static final int PADDING_SHORT = 27;

	public static void logRequest(HttpServletRequest request) {
		if (!RestfulStrolchComponent.getInstance().isRestLogging())
			return;

		try {
			StringBuilder sb = new StringBuilder();
			sb
					.append("\n")
					.append(pad("REQUEST URL:"))
					.append(request.getMethod())
					.append(" ")
					.append(request.getRequestURL())
					.append("\n");
			sb
					.append(pad("REQUEST:"))
					.append(request.getMethod())
					.append(" ")
					.append(request.getRequestURI())
					.append("\n");

			sb.append(pad("QUERY:")).append('?').append(string(request.getQueryString())).append("\n");

			String from = request.getRemoteAddr();
			sb.append(pad("REMOTE:")).append(from);
			if (!from.equals(request.getRemoteHost()))
				sb.append("/").append(request.getRemoteHost());
			sb.append(":").append(request.getRemotePort()).append("\n");

			Enumeration<String> headerNames = request.getHeaderNames();
			if (!headerNames.hasMoreElements()) {
				sb.append("HEADERS: (none)!\n");
			} else {
				sb.append("HEADERS: \n");
				while (headerNames.hasMoreElements()) {
					String headerName = headerNames.nextElement();
					String headerValue = request.getHeader(headerName);
					sb.append("  ").append(padShort(headerName)).append(" = ").append(headerValue).append("\n");
				}
			}

			Cookie[] cookies = request.getCookies();
			if (cookies == null) {
				sb.append("COOKIES: (none)!\n");
			} else {
				sb.append("COOKIES: \n");
				for (Cookie cookie : cookies) {
					sb
							.append("  ")
							.append(padShort(cookie.getName()))
							.append(" = ")
							.append(cookie.getValue())
							.append("\n");
				}
			}

			sb.append(pad("AuthType:")).append(string(request.getAuthType())).append("\n");
			sb.append(pad("User-Principal:")).append(string(request.getUserPrincipal())).append("\n");
			sb.append(pad("Remote User:")).append(string(request.getRemoteUser())).append("\n");
			sb.append(pad("Requested SessionID:")).append(string(request.getRequestedSessionId())).append("\n");
			sb.append(pad("Protocol:")).append(string(request.getProtocol())).append("\n");
			sb.append(pad("RequestId:")).append(string(request.getRequestId())).append("\n");
			sb.append(pad("DispatcherType:")).append(string(request.getDispatcherType())).append("\n");
			sb.append(pad("CharacterEncoding:")).append(string(request.getCharacterEncoding())).append("\n");
			sb.append(pad("ContentType:")).append(string(request.getContentType())).append("\n");
			sb.append(pad("ContentLength:")).append(request.getContentLengthLong()).append("\n");

			logger.info(sb.toString());
		} catch (Exception e) {
			logger.error("Failed to log request", e);
		}
	}

	private static String pad(String string) {
		return normalizeLength(string, PADDING, false, ' ');
	}

	private static String padShort(String string) {
		return normalizeLength(string, PADDING_SHORT, false, ' ');
	}

	private static String string(String string) {
		return isEmpty(string) ? "(none)" : string;
	}

	private static String string(Enum<?> e) {
		return e == null ? "(none)" : e.name();
	}

	private static String string(Object o) {
		return o == null ? "(none)" : o.toString();
	}
}
