package ch.eitchnet.communication.chat;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStreamReader;

import ch.eitchnet.communication.CommandKey;
import ch.eitchnet.communication.IoMessage;
import ch.eitchnet.communication.tcpip.SocketMessageVisitor;
import ch.eitchnet.utils.helper.StringHelper;

public class ChatMessageVisitor extends SocketMessageVisitor {

	public static final CommandKey inboundKey = CommandKey.key("server", "msg"); //$NON-NLS-1$//$NON-NLS-2$
	public static final CommandKey outboundKey = CommandKey.key("client", "msg"); //$NON-NLS-1$ //$NON-NLS-2$

	public ChatMessageVisitor(String connectionId) {
		super(connectionId);
	}

	@Override
	public IoMessage visit(DataInputStream inputStream, DataOutputStream outputStream) throws Exception {
		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
		String line = bufferedReader.readLine();
		ChatIoMessage chatIoMessage = new ChatIoMessage(StringHelper.getUniqueId(), inboundKey, this.connectionId);
		chatIoMessage.setChatMsg(line);
		return chatIoMessage;
	}

	@Override
	public void visit(DataInputStream inputStream, DataOutputStream outputStream, IoMessage message) throws Exception {
		ChatIoMessage chatIoMessage = (ChatIoMessage) message;
		outputStream.writeBytes(chatIoMessage.getChatMsg());
		outputStream.writeByte('\n');
		outputStream.flush();
	}
}
