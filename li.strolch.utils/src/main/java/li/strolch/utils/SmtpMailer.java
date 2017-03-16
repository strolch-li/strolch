package li.strolch.utils;

import java.io.UnsupportedEncodingException;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
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
 * <li><code>overrideRecipients</code> - if defined, overrides any recipients - useful for testng purposes</li>
 * <li><code>recipientWhitelist</code> - if defined allows those e-mail addresses to not be overridden</li>
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
	private final Set<InternetAddress> recipientWhitelist;
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
	 * @throws AddressException
	 *             if something goes wrong parsing the from or override addresses
	 */
	private SmtpMailer(Properties properties) throws UnsupportedEncodingException, AddressException {

		String fromAddr = properties.getProperty("fromAddr", null);
		String fromName = properties.getProperty("fromName", null);
		this.from = new InternetAddress(fromAddr, fromName);

		if (properties.containsKey("overrideRecipients")) {
			String addr = properties.getProperty("overrideRecipients", null);
			this.overrideRecipients = InternetAddress.parse(addr);
		} else {
			this.overrideRecipients = null;
		}

		if (!properties.containsKey("recipientWhitelist")) {
			this.recipientWhitelist = null;
		} else {
			String whiteListProp = properties.getProperty("recipientWhitelist");
			this.recipientWhitelist = Arrays.stream(whiteListProp.split(",")) //
					.map(s -> s.trim()) //
					.map(s -> Arrays.asList(parseAddress(s))) //
					.flatMap(List::stream) //
					.collect(Collectors.toSet());
		}

		this.username = properties.getProperty("username", null);
		this.password = properties.getProperty("password", null);

		this.props = new Properties();
		this.props.put("mail.smtp.auth", properties.getProperty("auth", null));
		this.props.put("mail.smtp.starttls.enable", properties.getProperty("startTls", null));
		this.props.put("mail.smtp.host", properties.getProperty("host", null));
		this.props.put("mail.smtp.port", properties.getProperty("port", null));
	}

	private InternetAddress[] parseAddress(String s) {
		try {
			return InternetAddress.parse(s);
		} catch (AddressException e) {
			throw new IllegalArgumentException("Failed to parse address: " + s, e);
		}
	}

	/**
	 * Sends an e-mail to given recipient (unless override address defined).
	 * 
	 * @param subject
	 *            the subject of the e-mail
	 * @param text
	 *            the test of the e-mail
	 * @param recipients
	 *            the addresses to whom to send the e-mail. See {@link InternetAddress#parse(String)}
	 */
	public void sendMail(String subject, String text, String recipients) {

		try {

			if (this.overrideRecipients == null) {

				// no override, just send
				send(subject, text, parseAddress(recipients));
				logger.info(MessageFormat.format("Sent E-mail to {0}: {1}", recipients, subject));

			} else if (this.recipientWhitelist == null) {

				// override, with no white list, so send to override
				text = "Override recipient. Original recipients: " + recipients + ".\n\n" + text;
				send(subject, text, this.overrideRecipients);
				logger.info(MessageFormat.format("Sent E-mail to override recipient {0}: {1}",
						Arrays.asList(this.overrideRecipients).stream() //
								.map(Object::toString) //
								.collect(Collectors.joining(",")),
						subject));

			} else {

				// override with whitelist, so we have to perhaps send to override and white list

				Set<InternetAddress> allowList = new HashSet<>();
				boolean needsOverrides = false;

				for (InternetAddress recipient : parseAddress(recipients)) {
					if (this.recipientWhitelist.contains(recipient))
						allowList.add(recipient);
					else
						needsOverrides = true;
				}

				if (!allowList.isEmpty()) {
					send(subject, text, parseAddress(recipients));
					logger.info(MessageFormat.format("Sent E-mail to {0}: {1}", recipients, subject));
				}

				if (needsOverrides) {

					// override, with no white list, so send to override
					text = "Override recipient. Original recipients: " + recipients + ".\n\n" + text;
					send(subject, text, this.overrideRecipients);
					logger.info(MessageFormat.format("Sent E-mail to override recipient {0}: {1}",
							Arrays.asList(this.overrideRecipients).stream() //
									.map(Object::toString) //
									.collect(Collectors.joining(",")),
							subject));
				}
			}

		} catch (MessagingException e) {
			throw new RuntimeException("Failed to send e-mail due to " + e.getMessage(), e);
		}
	}

	private void send(String subject, String text, InternetAddress[] recipients) throws MessagingException {

		Session session = Session.getInstance(this.props, new javax.mail.Authenticator() {
			protected PasswordAuthentication getPasswordAuthentication() {
				return new PasswordAuthentication(SmtpMailer.this.username, SmtpMailer.this.password);
			}
		});

		Message message = new MimeMessage(session);
		message.setFrom(this.from);
		message.addRecipients(Message.RecipientType.TO, recipients);
		message.setSubject(subject);
		message.setText(text);

		Transport.send(message);
	}
}
