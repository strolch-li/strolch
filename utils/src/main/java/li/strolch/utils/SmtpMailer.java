package li.strolch.utils;

import jakarta.mail.*;
import jakarta.mail.internet.*;
import li.strolch.utils.dbc.DBC;
import org.bouncycastle.openpgp.*;
import org.bouncycastle.openpgp.operator.PBESecretKeyDecryptor;
import org.bouncycastle.util.io.Streams;
import org.pgpainless.PGPainless;
import org.pgpainless.algorithm.DocumentSignatureType;
import org.pgpainless.algorithm.HashAlgorithm;
import org.pgpainless.algorithm.SymmetricKeyAlgorithm;
import org.pgpainless.encryption_signing.*;
import org.pgpainless.key.SubkeyIdentifier;
import org.pgpainless.key.protection.SecretKeyRingProtector;
import org.pgpainless.key.protection.UnlockSecretKey;
import org.pgpainless.util.Passphrase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.text.MessageFormat.format;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.joining;

/**
 * A simple helper class to send e-mails. Uses jakarta.mail and is built as a singleton, so configuration has to be done
 * only once.
 * <p>
 * When using a {@link Properties} to initialize, the following keys are defined:
 * <ul>
 * <li><code>fromAddr</code> - defines the address from which the e-mail comes from</li>
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

	public static final String PARAM_FROM_ADDR = "fromAddr";
	public static final String PARAM_USERNAME = "username";
	public static final String PARAM_PASSWORD = "password";
	public static final String PARAM_AUTH = "auth";
	public static final String PARAM_START_TLS = "startTls";
	public static final String PARAM_HOST = "host";
	public static final String PARAM_PORT = "port";

	public static final String MAIL_SMTP_PORT = "mail.smtp.port";
	public static final String MAIL_SMTP_HOST = "mail.smtp.host";
	public static final String MAIL_SMTP_STARTTLS_ENABLE = "mail.smtp.starttls.enable";
	public static final String MAIL_SMTP_AUTH = "mail.smtp.auth";
	public static final String PARAM_OVERRIDE_RECIPIENTS = "overrideRecipients";

	private final Properties props;
	private InternetAddress from;
	private Authenticator authenticator;
	private InternetAddress[] overrideRecipients;

	private PGPSecretKeyRing signingKeyRing;
	private char[] signingKeyPassword;
	private List<PGPPublicKeyRing> recipientKeyRings;

	private static SmtpMailer instance;

	/**
	 * <p>
	 * Initializes the SMTP Mailer with the given properties.
	 * </p>
	 *
	 * @param properties the properties to be used to initialize the mailer
	 */
	public static SmtpMailer init(Properties properties) {
		instance = new SmtpMailer(properties);
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
	public static SmtpMailer init(String fromAddress, String host, int port, boolean auth, boolean startTls,
			String username, String password) {
		instance = new SmtpMailer(fromAddress, host, port, auth, startTls, username, password);
		return instance;
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

	public SmtpMailer(Properties properties) {
		setFrom(properties.getProperty(PARAM_FROM_ADDR, null));

		boolean auth = Boolean.parseBoolean(properties.getProperty(PARAM_AUTH, null));
		if (auth) {
			String username = properties.getProperty(PARAM_USERNAME, null);
			String password = properties.getProperty(PARAM_PASSWORD, null);
			setAuthenticator(username, password);
		}

		if (properties.containsKey(PARAM_OVERRIDE_RECIPIENTS))
			setOverrideRecipients(properties.getProperty(PARAM_OVERRIDE_RECIPIENTS));

		this.props = new Properties();
		this.props.put(MAIL_SMTP_AUTH, auth);
		this.props.put(MAIL_SMTP_STARTTLS_ENABLE, properties.getProperty(PARAM_START_TLS, null));
		this.props.put(MAIL_SMTP_HOST, properties.getProperty(PARAM_HOST, null));
		this.props.put(MAIL_SMTP_PORT, properties.getProperty(PARAM_PORT, null));
	}

	public SmtpMailer(String fromAddress, String host, int port, boolean auth, boolean startTls, String username,
			String password) {

		setFrom(fromAddress);
		if (auth)
			setAuthenticator(username, password);

		this.props = new Properties();
		this.props.put(MAIL_SMTP_AUTH, auth);
		this.props.put(MAIL_SMTP_STARTTLS_ENABLE, startTls);
		this.props.put(MAIL_SMTP_HOST, host);
		this.props.put(MAIL_SMTP_PORT, port);
	}

	private void setFrom(String fromAddress) {
		try {
			this.from = InternetAddress.parse(fromAddress)[0];
		} catch (AddressException e) {
			throw new IllegalStateException("Illegal from address: " + fromAddress, e);
		}
	}

	private void setAuthenticator(String username, String password) {
		DBC.PRE.assertNotEmpty("username must not be empty!", username);
		DBC.PRE.assertNotNull("password must not be empty!", password);
		this.authenticator = new Authenticator() {
			protected PasswordAuthentication getPasswordAuthentication() {
				return new PasswordAuthentication(username, password);
			}
		};
	}

	public void setOverrideRecipients(String overrideRecipients) {
		try {
			this.overrideRecipients = InternetAddress.parse(overrideRecipients);
		} catch (AddressException e) {
			throw new IllegalArgumentException("Failed to parse override recipients: " + overrideRecipients, e);
		}
	}

	public void addRecipientPublicKeyFileName(String recipientPublicKeyFileName) {
		if (recipientKeyRings == null)
			this.recipientKeyRings = new ArrayList<>();
		this.recipientKeyRings.add(getRecipientKeyRing(recipientPublicKeyFileName));
	}

	public void setSigningKeyFileName(String signingKeyFileName, char[] signingKeyPassword) {
		// validate we can unlock the key with the given password
		PGPSecretKeyRing signingKeyRing = getSigningKeyRing(signingKeyFileName);
		try {
			PGPSecretKey secretKey = signingKeyRing.getSecretKey();
			UnlockSecretKey.unlockSecretKey(secretKey,
					SecretKeyRingProtector.unlockAnyKeyWith(new Passphrase(signingKeyPassword)));
		} catch (PGPException e) {
			throw new RuntimeException(e);
		}

		this.signingKeyRing = signingKeyRing;
		this.signingKeyPassword = signingKeyPassword;
	}

	/**
	 * Sends an e-mail to the given recipients (unless override address defined).
	 *
	 * @param recipients the addresses to whom to send the e-mail. See {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 */
	public void sendMail(String recipients, String subject, String text) {
		Session session = Session.getInstance(this.props, this.authenticator);

		MimeMessage message;
		InternetAddress[] recipientAddresses;
		try {
			recipientAddresses = evaluateRecipients(subject, recipients);
			message = prepareMimeMessage(subject, session);

			if (shouldSign()) {
				logger.info("Signing text with key {}", this.signingKeyRing.getPublicKey().getUserIDs().next());
				String signedMessage = sign(text);
				message.setText(signedMessage, UTF_8.name());
			} else {
				logger.info("Not signing text, as signing key not available.");
				message.setText(text, UTF_8.name());
			}
		} catch (Exception e) {
			throw new IllegalStateException("Failed to prepare message for sending!", e);
		}

		send(recipientAddresses, message);
		logger.info(format("Sent {0} E-mail with subject {1} to {2}", shouldSign() ? "signed" : "unsigned", subject,
				recipients));
	}

	private boolean shouldSign() {
		return this.signingKeyRing != null;
	}

	public void sendMailWithAttachment(String recipients, String subject, String text, String attachment,
			String fileName, String type) {
		Session session = Session.getInstance(this.props, this.authenticator);

		MimeMessage message;
		InternetAddress[] recipientAddresses;
		try {
			recipientAddresses = evaluateRecipients(subject, recipients);
			message = prepareMimeMessage(subject, session);

			MimeBodyPart messageBodyPart = new MimeBodyPart();
			messageBodyPart.setText(text);

			MimeBodyPart attachmentPart = new MimeBodyPart();
			attachmentPart.setContent(attachment, type);
			attachmentPart.setFileName(fileName);

			Multipart multipart = new MimeMultipart();
			multipart.addBodyPart(messageBodyPart);
			multipart.addBodyPart(attachmentPart);

			message.setContent(multipart);

		} catch (Exception e) {
			throw new IllegalStateException("Failed to prepare message for sending!", e);
		}

		send(recipientAddresses, message);
		logger.info(format("Sent {0} E-mail with subject {1} to {2} and attachment {3}",
				shouldSign() ? "signed" : "unsigned", subject, recipients, fileName));
	}

	public void sendEncryptedEmail(String recipients, String subject, String mailText, String secretText,
			String encryptedTextFileName) {
		DBC.PRE.assertNotNull("Encrypted e-mails require a signing key!", this.signingKeyRing);
		DBC.PRE.assertNotEmpty("Encrypted e-mails require at least one recipient key ring!", this.recipientKeyRings);
		Session session = Session.getInstance(this.props, this.authenticator);

		MimeMessage message;
		InternetAddress[] recipientAddresses;
		try {
			recipientAddresses = evaluateRecipients(subject, recipients);
			message = prepareMimeMessage(subject, session);
			attachEncryptedMessage(message, mailText, signAndEncrypt(secretText), encryptedTextFileName);
		} catch (Exception e) {
			throw new IllegalStateException("Failed to prepare message for sending!", e);
		}

		send(recipientAddresses, message);
		logger.info(format("Sent signed and encrypted E-mail with subject {0} to {1}", subject, recipients));
	}

	public void sendEncryptedEmailWithAttachment(String recipients, String subject, String mailText, String secretText,
			String encryptedTextFileName, String attachment, String fileName) {
		DBC.PRE.assertNotNull("Encrypted e-mails require a signing key!", this.signingKeyRing);
		DBC.PRE.assertNotEmpty("Encrypted e-mails require at least one recipient key ring!", this.recipientKeyRings);
		Session session = Session.getInstance(this.props, this.authenticator);

		MimeMessage message;
		InternetAddress[] recipientAddresses;
		try {
			recipientAddresses = evaluateRecipients(subject, recipients);
			message = prepareMimeMessage(subject, session);
			Multipart multipart = attachEncryptedMessage(message, mailText, signAndEncrypt(secretText),
					encryptedTextFileName);
			attachEncryptedFile(attachment, fileName, multipart);
		} catch (Exception e) {
			throw new IllegalStateException("Failed to prepare message for sending!", e);
		}

		send(recipientAddresses, message);
		logger.info(format("Sent signed and encrypted E-mail with subject {0} to {1}", subject, recipients));
	}

	protected void send(InternetAddress[] recipients, MimeMessage message) {
		try {
			Transport.send(message, recipients);
		} catch (MessagingException e) {
			throw new RuntimeException("Failed to send message to server %s with recipients %s".formatted(
					this.props.getProperty(MAIL_SMTP_HOST), addressesToString(recipients)), e);
		}
	}

	protected MimeMessage prepareMimeMessage(String subject, Session session) throws MessagingException {

		MimeMessage message;
		message = new MimeMessage(session);
		message.setFrom(this.from);
		message.setSubject(subject);
		return message;
	}

	protected InternetAddress[] evaluateRecipients(String subject, String recipients) {
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

	protected InternetAddress[] parseAddress(String s) {
		try {
			return InternetAddress.parse(s);
		} catch (AddressException e) {
			throw new IllegalArgumentException("Failed to parse address: " + s, e);
		}
	}

	protected String addressesToString(Address[] recipientAddresses) {
		return stream(recipientAddresses) //
				.map(Address::toString) //
				.collect(joining(","));
	}

	protected Multipart attachEncryptedMessage(MimeMessage message, String mailText, String encryptedAndSignedMessage,
			String encryptedTextFileName) throws MessagingException {

		String fileName = encryptedTextFileName + ".asc";

		MimeBodyPart textPart = new MimeBodyPart();
		textPart.setText(mailText, UTF_8.name());

		MimeBodyPart encryptedPart = new MimeBodyPart();
		encryptedPart.setFileName(fileName);
		encryptedPart.setContent(encryptedAndSignedMessage, "application/pgp-encrypted; name=\"" + fileName + "\"");
		encryptedPart.setDescription(fileName);
		encryptedPart.setDisposition("inline; filename=\"" + fileName + "\"");

		Multipart multiPart = new MimeMultipart();
		multiPart.addBodyPart(textPart);
		multiPart.addBodyPart(encryptedPart);

		message.setContent(multiPart);
		return multiPart;
	}

	protected void attachEncryptedFile(String attachment, String fileName, Multipart multipart)
			throws MessagingException {
		fileName = fileName + ".asc";

		MimeBodyPart attachmentPart = new MimeBodyPart();
		attachmentPart.setFileName(fileName);
		attachmentPart.setContent(signAndEncrypt(attachment),
				"application/pgp-encrypted; name=\"" + fileName + ".asc\"");
		attachmentPart.setDescription(fileName);
		attachmentPart.setDisposition("inline; filename=\"" + fileName + ".asc\"");
		multipart.addBodyPart(attachmentPart);
	}

	protected String sign(String plainText) {
		try {
			SigningOptions signingOptions = new SigningOptions()
					.addDetachedSignature(
							SecretKeyRingProtector.unlockAnyKeyWith(new Passphrase(this.signingKeyPassword)),
							this.signingKeyRing, DocumentSignatureType.BINARY_DOCUMENT)
					.overrideHashAlgorithm(HashAlgorithm.SHA256);

			ByteArrayOutputStream signatureResult = new ByteArrayOutputStream();
			EncryptionStream encryptionStream = PGPainless
					.encryptAndOrSign()
					.onOutputStream(signatureResult)
					.withOptions(ProducerOptions.sign(signingOptions).setCleartextSigned().setVersion(""));

			Streams.pipeAll(new ByteArrayInputStream(plainText.getBytes()), encryptionStream);
			encryptionStream.close();

			return signatureResult.toString(UTF_8);
		} catch (Exception e) {
			throw new IllegalStateException("Failed to sign plain text!", e);
		}
	}

	protected String signAndEncrypt(String plainText) {
		try {

			ByteArrayOutputStream signatureResult = new ByteArrayOutputStream();
			EncryptionStream encryptionStream = PGPainless
					.encryptAndOrSign()
					.onOutputStream(signatureResult)
					.withOptions(ProducerOptions
							.signAndEncrypt(getEncryptionOptions(), getSigningOptions())
							.setAsciiArmor(true)
							.setVersion(""));

			Streams.pipeAll(new ByteArrayInputStream(plainText.getBytes()), encryptionStream);
			encryptionStream.close();

			return signatureResult.toString(UTF_8);
		} catch (Exception e) {
			throw new IllegalStateException("Failed to encrypt and sign plain text!", e);
		}
	}

	protected SigningOptions getSigningOptions() throws PGPException {
		return new SigningOptions()
				.addInlineSignature(SecretKeyRingProtector.unlockAnyKeyWith(new Passphrase(this.signingKeyPassword)),
						this.signingKeyRing, DocumentSignatureType.BINARY_DOCUMENT)
				.overrideHashAlgorithm(HashAlgorithm.SHA256);
	}

	protected EncryptionOptions getEncryptionOptions() {
		return new EncryptionOptions()
				.addRecipients(this.recipientKeyRings)
				.overrideEncryptionAlgorithm(SymmetricKeyAlgorithm.AES_256);
	}

	protected PGPSecretKeyRing getSigningKeyRing(String signingKeyFileName) {
		PGPSecretKeyRing signingKeyRing;
		try {
			signingKeyRing = PGPainless.readKeyRing().secretKeyRing(new FileInputStream(signingKeyFileName));
		} catch (IOException e) {
			throw new IllegalStateException("Failed to read signing key " + signingKeyFileName, e);
		}

		if (signingKeyRing == null)
			throw new IllegalStateException("No secret key ring found for signing key file " + signingKeyFileName);
		return signingKeyRing;
	}

	protected PGPPublicKeyRing getRecipientKeyRing(String recipientPublicKeyFileName) {
		PGPPublicKeyRing recipientKeyRing;
		try {
			recipientKeyRing = PGPainless.readKeyRing().publicKeyRing(new FileInputStream(recipientPublicKeyFileName));
		} catch (IOException e) {
			throw new IllegalStateException("Failed to read recipient public key " + recipientPublicKeyFileName, e);
		}

		if (recipientKeyRing == null)
			throw new IllegalStateException("No public key found for recipient key file " + recipientPublicKeyFileName);
		return recipientKeyRing;
	}
}
