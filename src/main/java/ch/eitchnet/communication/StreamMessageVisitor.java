package ch.eitchnet.communication;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * <p>
 * {@link IoMessageVisitor} to read or write using IO Streams.
 * </p>
 * 
 * <p>
 * Concrete implementations must be thread safe!
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class StreamMessageVisitor extends IoMessageVisitor {

	public abstract void visit(OutputStream outputStream, IoMessage message) throws Exception;

	public abstract IoMessage visit(InputStream inputStream) throws Exception;
}
