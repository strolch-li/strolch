package li.strolch.privilege.handler;

import java.text.MessageFormat;

import li.strolch.privilege.model.internal.User;
import li.strolch.utils.SmtpMailer;
import li.strolch.utils.helper.StringHelper;

public class MailUserChallengeHandler extends UserChallengeHandler {

	private static final String EMAIL = "email";

	@Override
	public void sendChallengeToUser(User user, String challenge) {

		String subject = "Mail TAN";

		StringBuilder sb = new StringBuilder();
		sb.append("Hello ").append(user.getFirstname()).append(" ").append(user.getLastname()).append("\n\n");
		sb.append("You have requested an action which requires you to respond to a challenge.\n\n");
		sb.append("Please use the following code to response to the challenge:\n\n");
		sb.append(challenge);

		String text = sb.toString();
		String recipient = user.getProperty(EMAIL);
		if (StringHelper.isEmpty(recipient)) {
			String msg = "User {0} has no property {1}";
			throw new RuntimeException(MessageFormat.format(msg, user.getUsername(), EMAIL));
		}

		SmtpMailer.getInstance().sendMail(subject, text, recipient);
	}
}
