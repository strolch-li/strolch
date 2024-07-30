package li.strolch.utils;

import java.nio.charset.StandardCharsets;

/**
 * Copied from <a
 * href="https://github.com/ESAPI/esapi-java-legacy/blob/develop/src/main/java/org/owasp/esapi/reference/DefaultEncoder.java#L298">OWASP
 * ESAPI DefaultEncoder</a>
 */
public class LdapHelper {

	public static String encodeForLDAP(String input, boolean encodeWildcards) {
		if (input == null) {
			return null;
		}
		// should be replaced with LDAP codec
		StringBuilder sb = new StringBuilder();
		// According to Microsoft docs [1,2], the forward slash ('/') MUST be escaped.
		// According to RFC 4515 Section 3 [3], the forward slash (and other characters) MAY be escaped.
		// Since Microsoft is a MUST, escape forward slash for all implementations. Also see discussion at [4].
		// Characters above 0x7F are converted to UTF-8 and then hex encoded in the default case.
		// [1] https://docs.microsoft.com/en-us/windows/win32/adsi/search-filter-syntax
		// [2] https://social.technet.microsoft.com/wiki/contents/articles/5312.active-directory-characters-to-escape.aspx
		// [3] https://tools.ietf.org/search/rfc4515#section-3
		// [4] https://lists.openldap.org/hyperkitty/list/openldap-technical@openldap.org/thread/3QPDDLO356ONSJM3JUKD7NMPOOIKIQ5T/
		for (int i = 0; i < input.length(); i++) {
			char c = input.charAt(i);
			switch (c) {
				case '\\':
					sb.append("\\5c");
					break;
				case '/':
					sb.append("\\2f");
					break;
				case '*':
					if (encodeWildcards) {
						sb.append("\\2a");
					} else {
						sb.append(c);
					}

					break;
				case '(':
					sb.append("\\28");
					break;
				case ')':
					sb.append("\\29");
					break;
				case '\0':
					sb.append("\\00");
					break;
				default:
					if (c >= 0x80) {
						final byte[] u = String.valueOf(c).getBytes(StandardCharsets.UTF_8);
						for (byte b : u) {
							sb.append(String.format("\\%02x", b));
						}
					} else {
						sb.append(c);
					}
			}
		}
		return sb.toString();
	}

}
