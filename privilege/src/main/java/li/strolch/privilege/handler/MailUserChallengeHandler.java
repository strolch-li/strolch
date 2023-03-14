package li.strolch.privilege.handler;

import static li.strolch.privilege.base.PrivilegeConstants.EMAIL;

import java.text.MessageFormat;
import java.util.concurrent.CompletableFuture;

import li.strolch.privilege.model.internal.User;
import li.strolch.utils.SmtpMailer;
import li.strolch.utils.helper.StringHelper;

public class MailUserChallengeHandler extends UserChallengeHandler {

	@Override
	public void sendChallengeToUser(User user, String challenge) {

		String subject = "Mail TAN";

		StringBuilder sb = new StringBuilder();
		sb.append("Hello ").append(user.getFirstname()).append(" ").append(user.getLastname()).append("\n\n");
		sb.append("You have requested an action which requires you to respond to a challenge.\n\n");
		sb.append("Please use the following code to response to the challenge:\n\n");
		sb.append(challenge);

		String text = sb.toString();
		String recipient = user.getEmail();
		if (StringHelper.isEmpty(recipient)) {
			String msg = "User {0} has no property {1}, so can not initiate challenge!";
			logger.error(MessageFormat.format(msg, user.getUsername(), EMAIL));
			return;
		}

		// send e-mail async
		CompletableFuture.runAsync(() -> SmtpMailer.getInstance().sendMail(subject, text, recipient))
				.whenComplete((unused, throwable) -> logger.error("Failed to send e-mail for user " + user, throwable));
	}
}
