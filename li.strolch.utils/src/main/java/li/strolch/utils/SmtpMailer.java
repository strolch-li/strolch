package li.strolch.utils;

import java.io.UnsupportedEncodingException;
import java.text.MessageFormat;
import java.util.Properties;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A simple helper class to send e-mails. Uses javax.mail and is built as a singleton, so configuration has to be done
 * only once.
 * 
 * The {@link Properties} required are as follows:
 * <ul>
 * <li><code>fromAddr</code> and <code>fromName</code> - defines the address from which the e-mail comes from</li>
 * <li><code>overrideRecipientAddr</code> and <code>overrideRecipientName</code> - if defined, overrides any recipient -
 * useful for testng purposes</li>
 * <li>username - the username to authenticate at the SMTP Server</li>
 * <li>password - the password to authenticate at the SMTP Server</li>
 * <li>auth - boolean to define if auth is to be done</li>
 * <li>startTls - boolean to define if StartTLS is to be sent to the SMTP Server</li>
 * <li>host - the SMTP host name</li>
 * <li>port - the SMTP port</li>
 * </ul>
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class SmtpMailer {

	private static final Logger logger = LoggerFactory.getLogger(SmtpMailer.class);

	private static SmtpMailer instance;

	/**
	 * <p>
	 * Initializes the SMTP Mailer with the given properties.
	 * </p>
	 * 
	 * @param properties
	 *            the properties to be used to initialize the mailer
	 */
	public static void init(Properties properties) {
		try {
			instance = new SmtpMailer(properties);
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException("Failed to initialize Mailer due to " + e.getMessage(), e);
		}
	}

	/**
	 * Returns the instance
	 * 
	 * @return the instance
	 */
	public static SmtpMailer getInstance() {
		if (instance == null)
			throw new RuntimeException("Instance not yet configured! Call init first!");
		return instance;
	}

	private final InternetAddress from;
	private final InternetAddress overrideRecipient;
	private final String username;
	private final String password;
	private final Properties props;

	/**
	 * private constructor, use the {@link #init(Properties)}-method
	 * 
	 * @param properties
	 *            the properties to initialize the mailer
	 * 
	 * @throws UnsupportedEncodingException
	 *             if something goes wrong parsing the from or override addresses
	 */
	private SmtpMailer(Properties properties) throws UnsupportedEncodingException {

		String fromAddr = properties.getProperty("fromAddr", null);
		String fromName = properties.getProperty("fromName", null);
		this.from = new InternetAddress(fromAddr, fromName);

		if (properties.containsKey("overrideRecipientAddr")) {
			String addr = properties.getProperty("overrideRecipientAddr", null);
			String name = properties.getProperty("overrideRecipientName", null);
			this.overrideRecipient = new InternetAddress(addr, name);
		} else {
			this.overrideRecipient = null;
		}

		this.username = properties.getProperty("username", null);
		this.password = properties.getProperty("password", null);

		this.props = new Properties();
		props.put("mail.smtp.auth", properties.getProperty("auth", null));
		props.put("mail.smtp.starttls.enable", properties.getProperty("startTls", null));
		props.put("mail.smtp.host", properties.getProperty("host", null));
		props.put("mail.smtp.port", properties.getProperty("port", null));

	}

	/**
	 * Sends an e-mail to given recipient (unless override address defined).
	 * 
	 * @param subject
	 *            the subject of the e-mail
	 * @param text
	 *            the test of the e-mail
	 * @param recipient
	 *            the address to whom to send the e-mail
	 */
	public void sendMail(String subject, String text, String recipient) {

		try {

			Session session = Session.getInstance(this.props, new javax.mail.Authenticator() {
				protected PasswordAuthentication getPasswordAuthentication() {
					return new PasswordAuthentication(SmtpMailer.this.username, SmtpMailer.this.password);
				}
			});

			Message message = new MimeMessage(session);
			message.setFrom(this.from);
			if (this.overrideRecipient != null)
				message.setRecipient(Message.RecipientType.TO, this.overrideRecipient);
			else
				message.addRecipients(Message.RecipientType.TO, InternetAddress.parse(recipient));

			message.setSubject(subject);

			if (this.overrideRecipient != null)
				text = "Override recipient. Original recipient " + recipient + ".\n\n" + text;
			message.setText(text);

			Transport.send(message);

			String msg;
			if (this.overrideRecipient != null)
				msg = MessageFormat.format("Sent E-mail to override recipient {0}: {1}",
						message.getRecipients(Message.RecipientType.TO)[0], message.getSubject());
			else {
				msg = MessageFormat.format("Sent E-mail to {0}: {1}",
						message.getRecipients(Message.RecipientType.TO)[0], message.getSubject());
			}
			logger.info(msg);

		} catch (MessagingException e) {
			throw new RuntimeException("Failed to send e-mail due to " + e.getMessage(), e);
		}
	}
}
