package ch.eitchnet.communication.chat;

import ch.eitchnet.communication.CommandKey;
import ch.eitchnet.communication.IoMessage;
import ch.eitchnet.utils.helper.StringHelper;

public class ChatIoMessage extends IoMessage {

	private String chatMsg;

	public ChatIoMessage(String id, CommandKey key, String connectionId) {
		super(id, key, connectionId);
	}

	public String getChatMsg() {
		return this.chatMsg;
	}

	public void setChatMsg(String chagMsg) {
		this.chatMsg = chagMsg;
	}

	public static ChatIoMessage msg(CommandKey key, String connId, String username, String msg) {

		String line = username + StringHelper.COLON + StringHelper.SPACE + msg;

		ChatIoMessage chatIoMessage = new ChatIoMessage(StringHelper.getUniqueId(), key, connId);
		chatIoMessage.setChatMsg(line);
		return chatIoMessage;
	}
}
