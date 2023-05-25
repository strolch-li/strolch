package li.strolch.utils;

import jakarta.mail.*;
import jakarta.mail.internet.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.util.Properties;

import static java.text.MessageFormat.format;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.joining;

/**
 * A simple helper class to send e-mails. Uses jakarta.mail and is built as a singleton, so configuration has to be done
 * only once.
 * <p>
 * The {@link Properties} required are as follows:
 * <ul>
 * <li><code>fromAddr</code> and <code>fromName</code> - defines the address from which the e-mail comes from</li>
 * <li><code>overrideRecipients</code> - if defined, overrides any recipients - useful for testng purposes</li>
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
	 * @param properties the properties to be used to initialize the mailer
	 */
	public static void init(Properties properties) {
		try {
			instance = new SmtpMailer(properties);
		} catch (UnsupportedEncodingException | AddressException e) {
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
	private final InternetAddress[] overrideRecipients;
	private final String username;
	private final String password;
	private final Properties props;

	/**
	 * private constructor, use the {@link #init(Properties)}-method
	 *
	 * @param properties the properties to initialize the mailer
	 *
	 * @throws UnsupportedEncodingException if something goes wrong parsing the from or override addresses
	 * @throws AddressException             if something goes wrong parsing the from or override addresses
	 */
	private SmtpMailer(Properties properties) throws UnsupportedEncodingException, AddressException {

		String fromAddr = properties.getProperty("fromAddr", null);
		String fromName = properties.getProperty("fromName", null);
		this.from = new InternetAddress(fromAddr, fromName);

		if (!properties.containsKey("overrideRecipients")) {
			this.overrideRecipients = null;
		} else {
			String addr = properties.getProperty("overrideRecipients", null);
			this.overrideRecipients = InternetAddress.parse(addr);
		}

		this.username = properties.getProperty("username", null);
		this.password = properties.getProperty("password", null);

		this.props = new Properties();
		this.props.put("mail.smtp.auth", properties.getProperty("auth", null));
		this.props.put("mail.smtp.starttls.enable", properties.getProperty("startTls", null));
		this.props.put("mail.smtp.host", properties.getProperty("host", null));
		this.props.put("mail.smtp.port", properties.getProperty("port", null));
	}

	/**
	 * Sends an e-mail to the given recipients (unless override address defined).
	 *
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param recipients the addresses to whom to send the e-mail. See {@link InternetAddress#parse(String)}
	 */
	public void sendMail(String subject, String text, String recipients) {
		try {
			InternetAddress[] recipientAddresses = evaluateRecipients(subject, recipients);
			Message message = buildMessage(subject, recipientAddresses);
			message.setText(text);

			Transport.send(message);
			logger.info(format("Sent E-mail with subject {0} to {1}", subject, addressesToString(recipientAddresses)));

		} catch (MessagingException e) {
			logger.error("Failed to send the following e-mail:\nSubject: " + subject + "\nRecipients: " + recipients +
					"\n\nBody:\n" + text);
			throw new RuntimeException("Failed to send e-mail due to " + e.getMessage(), e);
		}
	}

	/**
	 * Sends an e-mail with an attachment to the given recipients (unless override address defined).
	 *
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param recipients the addresses to whom to send the e-mail. See {@link InternetAddress#parse(String)}
	 */
	public void sendMail(String subject, String text, String recipients, String attachment, String fileName,
						 String type) {
		try {
			InternetAddress[] recipientAddresses = evaluateRecipients(subject, recipients);
			Message message = buildMessage(subject, recipientAddresses);

			MimeBodyPart messageBodyPart = new MimeBodyPart();
			messageBodyPart.setContent(text, "text/html; charset=utf-8");

			MimeBodyPart attachmentPart = new MimeBodyPart();
			attachmentPart.setContent(attachment, type);
			attachmentPart.setFileName(fileName);

			Multipart multipart = new MimeMultipart();
			multipart.addBodyPart(messageBodyPart);
			multipart.addBodyPart(attachmentPart);

			message.setContent(multipart);

			Transport.send(message);
		} catch (MessagingException e) {
			logger.error("Failed to send the following e-mail:\nSubject: " + subject + "\nAttachment: " + fileName +
					"\nRecipients: " + recipients + "\n\nBody:\n" + text);
			throw new RuntimeException("Failed to send e-mail due to " + e.getMessage(), e);
		}
	}

	private Message buildMessage(String subject, InternetAddress[] recipients) throws MessagingException {
		Session session = Session.getInstance(this.props, getAuthenticator());

		Message message = new MimeMessage(session);
		message.setFrom(this.from);
		message.addRecipients(Message.RecipientType.TO, recipients);
		message.setSubject(subject);
		return message;
	}

	private Authenticator getAuthenticator() {
		return new Authenticator() {
			protected PasswordAuthentication getPasswordAuthentication() {
				return new PasswordAuthentication(SmtpMailer.this.username, SmtpMailer.this.password);
			}
		};
	}

	private InternetAddress[] parseAddress(String s) {
		try {
			return InternetAddress.parse(s);
		} catch (AddressException e) {
			throw new IllegalArgumentException("Failed to parse address: " + s, e);
		}
	}

	private InternetAddress[] evaluateRecipients(String subject, String recipients) {
		InternetAddress[] recipientAddresses;
		if (this.overrideRecipients == null) {
			recipientAddresses = parseAddress(recipients);
			logger.info(
					format("Sending e-mail with subject {0} to {1}", subject, addressesToString(recipientAddresses)));
		} else {
			recipientAddresses = this.overrideRecipients;
			logger.info(format("Sending e-mail with subject {0} to override recipient {1}", subject,
					addressesToString(recipientAddresses)));
		}
		return recipientAddresses;
	}

	private String addressesToString(InternetAddress[] recipientAddresses) {
		return stream(recipientAddresses) //
				.map(Object::toString) //
				.collect(joining(","));
	}
}
