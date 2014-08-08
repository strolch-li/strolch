package ch.eitchnet.communication.console;

import org.slf4j.Logger;

import ch.eitchnet.communication.IoMessage;
import ch.eitchnet.communication.IoMessageVisitor;

public abstract class ConsoleMessageVisitor extends IoMessageVisitor {

	public abstract void visit(Logger logger, IoMessage message) throws Exception;
}
