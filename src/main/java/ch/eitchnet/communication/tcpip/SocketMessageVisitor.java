package ch.eitchnet.communication.tcpip;

import java.io.DataInputStream;
import java.io.DataOutputStream;

import ch.eitchnet.communication.IoMessage;
import ch.eitchnet.communication.IoMessageVisitor;

public abstract class SocketMessageVisitor extends IoMessageVisitor {

	public abstract IoMessage visit(DataInputStream inputStream, DataOutputStream outputStream) throws Exception;

	public abstract void visit(DataInputStream inputStream, DataOutputStream outputStream, IoMessage message)
			throws Exception;
}
