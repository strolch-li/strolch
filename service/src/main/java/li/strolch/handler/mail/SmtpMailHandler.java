package li.strolch.handler.mail;

import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

import java.util.ResourceBundle;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.SmtpMailer;

/**
 * SMTP {@link MailHandler}. Uses {@link SmtpMailer}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 * @see SmtpMailer
 */
public class SmtpMailHandler extends MailHandler {

	private String realm;
	private String host;

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
		this.realm = configuration.getString("realm", getContainer().getRealmNames().iterator().next());
		this.host = configuration.getString("host", null);
		// mark certain properties as secret
		configuration.getSecret("password");
		SmtpMailer.init(configuration.getAsProperties());
		super.initialize(configuration);
	}

	@Override
	public void sendMail(String subject, String text, String recipients) {
		SmtpMailer.getInstance().sendMail(subject, text, recipients);
	}

	@Override
	public void sendMail(String subject, String text, String recipients, String attachment, String fileName,
			String type) {
		SmtpMailer.getInstance().sendMail(subject, text, recipients, attachment, fileName, type);
	}

	@Override
	public void sendMailAsync(String subject, String text, String recipients) {
		getExecutorService("Mail").submit(() -> doSendMail(subject, text, recipients));
	}

	@Override
	public void sendMailAsync(String subject, String text, String recipients, String attachment, String fileName,
			String type) {
		getExecutorService("Mail").submit(() -> doSendMail(subject, text, recipients, attachment, fileName, type));
	}

	private void doSendMail(String subject, String text, String recipients) {
		try {
			SmtpMailer.getInstance().sendMail(subject, text, recipients);
		} catch (Throwable e) {
			logger.error("Failed to send mail \"{}\" to {}", subject, recipients, e);

			if (hasComponent(OperationsLog.class)) {
				LogMessage message = new LogMessage(this.realm, SYSTEM_USER_AGENT, getLocator(), LogSeverity.Exception,
						LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
						"mail.failedToSend").withException(e).value("reason", e).value("host", this.host)
						.value("subject", subject).value("recipients", recipients);
				getComponent(OperationsLog.class).addMessage(message);
			}
		}
	}

	private void doSendMail(String subject, String text, String recipients, String attachment, String fileName,
			String type) {
		try {
			SmtpMailer.getInstance().sendMail(subject, text, recipients, attachment, fileName, type);
		} catch (Throwable e) {
			logger.error("Failed to send mail \"{}\" to {} with attachment {}", subject, recipients, fileName, e);

			if (hasComponent(OperationsLog.class)) {
				LogMessage message = new LogMessage(this.realm, SYSTEM_USER_AGENT, getLocator(), LogSeverity.Exception,
						LogMessageState.Information, ResourceBundle.getBundle("strolch-service"),
						"mail.failedToSendWithAttachment").withException(e).value("reason", e)
						.value("fileName", fileName).value("host", this.host).value("subject", subject)
						.value("recipients", recipients);
				getComponent(OperationsLog.class).addMessage(message);
			}
		}
	}
}
