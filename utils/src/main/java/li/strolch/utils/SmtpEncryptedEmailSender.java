package li.strolch.utils;

import jakarta.mail.*;
import jakarta.mail.internet.*;
import org.bouncycastle.openpgp.PGPPublicKeyRing;
import org.bouncycastle.openpgp.PGPSecretKeyRing;
import org.bouncycastle.util.io.Streams;
import org.pgpainless.PGPainless;
import org.pgpainless.algorithm.DocumentSignatureType;
import org.pgpainless.algorithm.HashAlgorithm;
import org.pgpainless.algorithm.SymmetricKeyAlgorithm;
import org.pgpainless.encryption_signing.EncryptionOptions;
import org.pgpainless.encryption_signing.EncryptionStream;
import org.pgpainless.encryption_signing.ProducerOptions;
import org.pgpainless.encryption_signing.SigningOptions;
import org.pgpainless.key.protection.SecretKeyRingProtector;
import org.pgpainless.util.Passphrase;

import java.io.*;
import java.util.Properties;

import static java.nio.charset.StandardCharsets.UTF_8;

public class SmtpEncryptedEmailSender {

	public static final String MAIL_SMTP_PORT = "mail.smtp.port";
	public static final String MAIL_SMTP_HOST = "mail.smtp.host";
	public static final String MAIL_SMTP_STARTTLS_ENABLE = "mail.smtp.starttls.enable";
	public static final String MAIL_SMTP_AUTH = "mail.smtp.auth";

	private final InternetAddress from;
	private final Properties props;
	private final Authenticator authenticator;

	private String recipientPublicKeyFileName;
	private String signingKeyFileName;
	private char[] signingKeyPassword;

	public SmtpEncryptedEmailSender(String fromAddress, String host, int port, boolean auth, boolean startTls, String username,
			String password) {
		try {
			this.from = InternetAddress.parse(fromAddress)[0];
		} catch (AddressException e) {
			throw new IllegalStateException("Illegal from address: " + fromAddress, e);
		}
		this.props = new Properties();
		this.props.put(MAIL_SMTP_AUTH, auth);
		this.props.put(MAIL_SMTP_STARTTLS_ENABLE, startTls);
		this.props.put(MAIL_SMTP_HOST, host);
		this.props.put(MAIL_SMTP_PORT, port);

		this.authenticator = new Authenticator() {
			protected PasswordAuthentication getPasswordAuthentication() {
				return new PasswordAuthentication(username, password);
			}
		};
	}

	public SmtpEncryptedEmailSender(Properties properties) {
		String fromAddr = properties.getProperty("fromAddr", null);
		String fromName = properties.getProperty("fromName", null);
		try {
			this.from = new InternetAddress(fromAddr, fromName);
		} catch (UnsupportedEncodingException e) {
			throw new IllegalStateException("Illegal from address: " + fromAddr + " with name: " + fromName, e);
		}

		String username = properties.getProperty("username", null);
		String password = properties.getProperty("password", null);
		this.authenticator = new Authenticator() {
			protected PasswordAuthentication getPasswordAuthentication() {
				return new PasswordAuthentication(username, password);
			}
		};

		this.props = new Properties();
		this.props.put(MAIL_SMTP_AUTH, properties.getProperty("auth", null));
		this.props.put(MAIL_SMTP_STARTTLS_ENABLE, properties.getProperty("startTls", null));
		this.props.put(MAIL_SMTP_HOST, properties.getProperty("host", null));
		this.props.put(MAIL_SMTP_PORT, properties.getProperty("port", null));
	}

	public void setRecipientPublicKeyFileName(String recipientPublicKeyFileName) {
		this.recipientPublicKeyFileName = recipientPublicKeyFileName;
	}

	public void setSigningKeyFileName(String signingKeyFileName) {
		this.signingKeyFileName = signingKeyFileName;
	}

	public void setSigningKeyPassword(char[] signingKeyPassword) {
		this.signingKeyPassword = signingKeyPassword;
	}

	public void sendSignedAndEncryptedEmail(String recipientEmail, String subject, String mailText, String plainText,
			String fileName) {
		Session session = Session.getInstance(this.props, this.authenticator);

		// Create the email message
		MimeMessage message = new MimeMessage(session);
		try {
			message.setFrom(this.from);
			message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(recipientEmail));
			message.setSubject(subject);

			String encryptedAndSignedMessage = signAndEncrypt(plainText);

			Multipart multiPart = getMultipart(mailText, fileName, encryptedAndSignedMessage);
			message.setContent(multiPart);

		} catch (Exception e) {
			throw new IllegalStateException("Failed to prepare message for sending!", e);
		}

		// Send the message
		try {
			Transport.send(message);
		} catch (MessagingException e) {
			throw new RuntimeException("Failed to send message to " + this.props.getProperty(MAIL_SMTP_HOST), e);
		}
	}

	private static Multipart getMultipart(String mailText, String fileName, String encryptedAndSignedMessage)
			throws MessagingException {

		MimeBodyPart textPart = new MimeBodyPart();
		textPart.setText(mailText, UTF_8.name());

		MimeBodyPart encryptedPart = new MimeBodyPart();
		encryptedPart.setFileName(fileName);
		encryptedPart.setContent(encryptedAndSignedMessage, "application/pgp-encrypted; name=\"" + fileName + "\"");
		encryptedPart.setDescription("OpenPGP encrypted message");
		encryptedPart.setDisposition("inline; filename=\"" + fileName + "\"");

		Multipart multiPart = new MimeMultipart();
		multiPart.addBodyPart(textPart);
		multiPart.addBodyPart(encryptedPart);
		return multiPart;
	}

	private String signAndEncrypt(String plainText) {
		// for the signature, add new lines
		plainText += "\n\n";

		ByteArrayInputStream inputStream = new ByteArrayInputStream(plainText.getBytes());

		PGPPublicKeyRing recipientKeyRing = getRecipientKeyRing();
		PGPSecretKeyRing signingKeyRing = getSigningKeyRing();

		ByteArrayOutputStream signatureResult = new ByteArrayOutputStream();

		// If you use a single passphrase for all (sub-) keys, take this:
		SecretKeyRingProtector secretKeyDecryptor = SecretKeyRingProtector.unlockAnyKeyWith(
				new Passphrase(this.signingKeyPassword));

		try {
			EncryptionStream encryptionStream = PGPainless
					.encryptAndOrSign()
					.onOutputStream(signatureResult)
					.withOptions(ProducerOptions.signAndEncrypt(new EncryptionOptions().addRecipient(recipientKeyRing)
							// optionally override symmetric encryption algorithm
							.overrideEncryptionAlgorithm(SymmetricKeyAlgorithm.AES_256), new SigningOptions()
							// Sign in-line (using one-pass-signature packet)
							.addInlineSignature(secretKeyDecryptor, signingKeyRing,
									DocumentSignatureType.BINARY_DOCUMENT)
							// optionally override hash algorithm
							.overrideHashAlgorithm(HashAlgorithm.SHA256)).setAsciiArmor(true) // Ascii armor or not
					);

			Streams.pipeAll(inputStream, encryptionStream);
			encryptionStream.close();

			return signatureResult.toString(UTF_8);
		} catch (Exception e) {
			throw new IllegalStateException("Failed to encrypt and sign plain text!", e);
		}
	}

	private PGPSecretKeyRing getSigningKeyRing() {
		PGPSecretKeyRing signingKeyRing;
		try {
			signingKeyRing = PGPainless.readKeyRing().secretKeyRing(new FileInputStream(this.signingKeyFileName));
		} catch (IOException e) {
			throw new IllegalStateException("Failed to read signing key " + this.signingKeyFileName, e);
		}
		if (signingKeyRing == null)
			throw new IllegalStateException("No secret key ring found for signing key file " + this.signingKeyFileName);
		return signingKeyRing;
	}

	private PGPPublicKeyRing getRecipientKeyRing() {
		PGPPublicKeyRing recipientKeyRing;
		try {
			recipientKeyRing = PGPainless
					.readKeyRing()
					.publicKeyRing(new FileInputStream(this.recipientPublicKeyFileName));
		} catch (IOException e) {
			throw new IllegalStateException("Failed to read recipient public key " + this.recipientPublicKeyFileName,
					e);
		}
		if (recipientKeyRing == null)
			throw new IllegalStateException("No public key found for recipient key file " + recipientPublicKeyFileName);
		return recipientKeyRing;
	}
}