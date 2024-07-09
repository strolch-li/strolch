package li.strolch.handler.mail;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.SmtpMailer;

import java.io.File;
import java.util.List;
import java.util.ResourceBundle;

import static li.strolch.privilege.base.PrivilegeConstants.REALM;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

/**
 * SMTP {@link MailHandler}. Uses {@link SmtpMailer}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 * @see SmtpMailer
 */
public class SmtpMailHandler extends MailHandler {

	public static final String PARAM_SIGNING_KEY = "signingKey";
	public static final String PARAM_SIGNING_KEY_PASSWORD = "signingKeyPassword";
	public static final String PARAM_RECIPIENT_PUBLIC_KEYS = "recipientPublicKeys";

	private String realm;
	private String host;
	private boolean encryptionEnabled;
	private boolean signingEnabled;

	public SmtpMailHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	/**
	 * See {@link SmtpMailer} for which configuration properties are required
	 *
	 * @see SmtpMailer
	 */
	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {
		this.realm = configuration.getString(REALM, getContainer().getRealmNames().iterator().next());
		this.host = configuration.getString(SmtpMailer.PARAM_HOST, null);

		// mark certain properties as secret
		configuration.getSecret(SmtpMailer.PARAM_PASSWORD);

		SmtpMailer smtpMailer = SmtpMailer.init(configuration.getAsProperties());

		File configPath = configuration.getRuntimeConfiguration().getConfigPath();
		if (!configuration.hasProperty(PARAM_SIGNING_KEY)) {
			logger.info("Signing of emails is not enabled!");
		} else {

			String signingKeyPassword = configuration.getSecret(PARAM_SIGNING_KEY_PASSWORD);
			File signingKey = new File(configPath, configuration.getString(PARAM_SIGNING_KEY, null));
			if (!signingKey.exists())
				throw new IllegalArgumentException("Can not find signing key file: " + signingKey);
			smtpMailer.setSigningKeyFileName(signingKey.getAbsolutePath(), signingKeyPassword.toCharArray());
			signingEnabled = true;
			logger.info("Enabled signing of emails with key {}", signingKey.getAbsolutePath());
		}

		if (configuration.hasProperty(PARAM_RECIPIENT_PUBLIC_KEYS)) {
			if (!this.signingEnabled)
				throw new IllegalStateException(
						"Can not enable encryption without a signing key! Please set configuration property %s and %s".formatted(
								PARAM_SIGNING_KEY, PARAM_SIGNING_KEY_PASSWORD));

			logger.info("Enabling encryption for the following recipients:");
			List<String> recipientPublicKeys = configuration.getStringList(PARAM_RECIPIENT_PUBLIC_KEYS, "");
			if (recipientPublicKeys.isEmpty())
				throw new IllegalStateException(
						"At least one recipient key must be defined in " + PARAM_RECIPIENT_PUBLIC_KEYS);
			for (String recipientPublicKey : recipientPublicKeys) {
				logger.info("  {}", recipientPublicKey);
				File recipientPublicKeyFile = new File(configPath, recipientPublicKey);
				if (!recipientPublicKeyFile.exists())
					throw new IllegalStateException("Can not find recipient public key file: " + recipientPublicKey);
				smtpMailer.addRecipientPublicKeyFileName(recipientPublicKeyFile.getAbsolutePath());
			}

			this.encryptionEnabled = true;
		}

		super.initialize(configuration);
	}

	public boolean isSigningEnabled() {
		return this.signingEnabled;
	}

	public boolean isEncryptionEnabled() {
		return this.encryptionEnabled;
	}

	@Override
	public void sendMail(String subject, String text, String recipients) {
		SmtpMailer.getInstance().sendMail(subject, text, recipients);
	}

	public void sendMailWithAttachment(String subject, String text, String recipients, String attachment,
			String fileName, String type) {
		SmtpMailer.getInstance().sendMailWithAttachment(subject, text, recipients, attachment, fileName, type);
	}

	@Override
	public void sendMailAsync(String subject, String text, String recipients) {
		getExecutorService("Mail").submit(() -> doSendMail(recipients, subject, text));
	}

	@Override
	public void sendMailWithAttachmentAsync(String subject, String text, String recipients, String attachment,
			String fileName, String type) {
		getExecutorService("Mail").submit(
				() -> doSendMailWithAttachment(recipients, subject, text, attachment, fileName, type));
	}

	private void doSendMail(String recipients, String subject, String text) {
		try {
			SmtpMailer.getInstance().sendMail(recipients, subject, text);
		} catch (Throwable e) {
			logger.error("Failed to send mail \"{}\" to {}", subject, recipients, e);

			if (hasComponent(OperationsLog.class)) {
				LogMessage message = new LogMessage(this.realm, SYSTEM_USER_AGENT, getLocator(), LogSeverity.Exception,
						LogMessageState.Information, ResourceBundle.getBundle("strolch-service"), "mail.failedToSend")
						.withException(e)
						.value("reason", e)
						.value("host", this.host)
						.value("subject", subject)
						.value("recipients", recipients);
				getComponent(OperationsLog.class).addMessage(message);
			}
		}
	}

	private void doSendMailWithAttachment(String recipients, String subject, String text, String attachment,
			String fileName, String type) {
		try {
			SmtpMailer.getInstance().sendMailWithAttachment(subject, text, recipients, attachment, fileName, type);
		} catch (Throwable e) {
			logger.error("Failed to send mail \"{}\" to {} with attachment {}", subject, recipients, fileName, e);

			if (hasComponent(OperationsLog.class)) {
				LogMessage message = new LogMessage(this.realm, SYSTEM_USER_AGENT, getLocator(), LogSeverity.Exception,
						LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
						"mail.failedToSendWithAttachment")
						.withException(e)
						.value("reason", e)
						.value("fileName", fileName)
						.value("host", this.host)
						.value("subject", subject)
						.value("recipients", recipients);
				getComponent(OperationsLog.class).addMessage(message);
			}
		}
	}
}
