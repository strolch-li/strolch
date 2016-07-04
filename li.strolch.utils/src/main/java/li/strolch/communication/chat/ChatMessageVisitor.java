package li.strolch.communication.chat;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStreamReader;

import li.strolch.communication.CommandKey;
import li.strolch.communication.IoMessage;
import li.strolch.communication.tcpip.SocketMessageVisitor;
import li.strolch.utils.helper.StringHelper;

public class ChatMessageVisitor extends SocketMessageVisitor {

	public static final CommandKey inboundKey = CommandKey.key("server", "msg"); //$NON-NLS-1$//$NON-NLS-2$
	public static final CommandKey outboundKey = CommandKey.key("client", "msg"); //$NON-NLS-1$ //$NON-NLS-2$

	public ChatMessageVisitor(String connectionId) {
		super(connectionId);
	}

	@Override
	public IoMessage visit(DataInputStream inputStream, DataOutputStream outputStream) throws Exception {

		@SuppressWarnings("resource")
		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
		String line = bufferedReader.readLine();
		if (line == null) {
			bufferedReader.close();
			return null;
		}

		ChatIoMessage chatIoMessage = new ChatIoMessage(StringHelper.getUniqueId(), inboundKey, this.connectionId);
		chatIoMessage.setChatMsg(line);
		return chatIoMessage;
	}

	@Override
	public void visit(DataInputStream inputStream, DataOutputStream outputStream, IoMessage message) throws Exception {
		ChatIoMessage chatIoMessage = (ChatIoMessage) message;
		outputStream.writeChars(chatIoMessage.getChatMsg());
		outputStream.writeChar('\n');
		outputStream.flush();
	}
}
