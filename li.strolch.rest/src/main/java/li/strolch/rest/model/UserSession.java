package li.strolch.rest.model;

import java.util.Date;
import java.util.Locale;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import ch.eitchnet.privilege.model.Certificate;

@XmlRootElement(name = "UserSession")
@XmlAccessorType(XmlAccessType.NONE)
public class UserSession {

	@XmlAttribute(name = "sessionId")
	private String sessionId;
	@XmlAttribute(name = "loginTime")
	private Date loginTime;
	@XmlAttribute(name = "username")
	private String username;
	@XmlAttribute(name = "firstname")
	private String firstname;
	@XmlAttribute(name = "lastname")
	private String lastname;
	@XmlElement(name = "roles")
	private Set<String> userRoles;
	@XmlAttribute(name = "locale")
	private Locale locale;
	@XmlAttribute(name = "lastAccess")
	private Date lastAccess;

	public UserSession() {
		// no-arg constructor for JAXB
	}

	public UserSession(Certificate certificate) {
		this.sessionId = certificate.getSessionId();
		this.loginTime = certificate.getLoginTime();
		this.username = certificate.getUsername();
		this.firstname = certificate.getFirstname();
		this.lastname = certificate.getLastname();
		this.userRoles = certificate.getUserRoles();
		this.locale = certificate.getLocale();
		this.lastAccess = certificate.getLastAccess();
	}

	public Locale getLocale() {
		return locale;
	}

	public Date getLastAccess() {
		return lastAccess;
	}

	public String getSessionId() {
		return sessionId;
	}

	public Date getLoginTime() {
		return loginTime;
	}

	public String getUsername() {
		return username;
	}

	public String getFirstname() {
		return firstname;
	}

	public String getLastname() {
		return lastname;
	}

	public Set<String> getUserRoles() {
		return userRoles;
	}
}
