package li.strolch.utils;

import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.util.Properties;

public class SimulatedSmtpMailer extends SmtpMailer {

	private static final Logger logger = LoggerFactory.getLogger(SimulatedSmtpMailer.class);
	private static SimulatedSmtpMailer instance;

	/**
	 * <p>
	 * Initializes the SMTP Mailer with the given properties.
	 * </p>
	 *
	 * @param properties the properties to be used to initialize the mailer
	 */
	public static SimulatedSmtpMailer init(Properties properties) {
		instance = new SimulatedSmtpMailer(properties);
		return instance;
	}

	/**
	 * <p>
	 * Initializes the SMTP Mailer with the given properties.
	 * </p>
	 *
	 * @param fromAddress the from address
	 * @param host        the host to send the mails to
	 * @param port        the host port
	 * @param auth        boolean to indicate the connection to the host requires auth
	 * @param startTls    should STARTTLS be send to the host
	 * @param username    the username for connection authorization
	 * @param password    the password for connection authorization
	 *
	 * @return
	 */
	public static SimulatedSmtpMailer init(String fromAddress, String host, int port, boolean auth, boolean startTls,
			String username, String password) {
		instance = new SimulatedSmtpMailer(fromAddress, host, port, auth, startTls, username, password);
		return instance;
	}

	/**
	 * Returns the instance
	 *
	 * @return the instance
	 */
	public static SimulatedSmtpMailer getInstance() {
		if (instance == null)
			throw new RuntimeException("Instance not yet configured! Call init first!");
		return instance;
	}

	public SimulatedSmtpMailer(Properties properties) {
		super(properties);
	}

	public SimulatedSmtpMailer(String fromAddress, String host, int port, boolean auth, boolean startTls,
			String username, String password) {
		super(fromAddress, host, port, auth, startTls, username, password);
	}

	@Override
	protected void send(InternetAddress[] recipients, MimeMessage message) {
		try {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			message.writeTo(out);
			logger.info("Simulated sending of the following message to recipients: {}:\n{}",
					addressesToString(recipients), out);
		} catch (Exception e) {
			throw new IllegalStateException("Failed to print message!", e);
		}
	}
}
