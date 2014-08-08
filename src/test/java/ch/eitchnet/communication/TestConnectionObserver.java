package ch.eitchnet.communication;

import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestConnectionObserver implements ConnectionObserver {

	private static final Logger logger = LoggerFactory.getLogger(FileEndpointTest.class);

	private IoMessage message;

	public IoMessage getMessage() {
		return this.message;
	}

	@Override
	public void notify(CommandKey key, IoMessage message) {
		this.message = message;
		logger.info(MessageFormat.format("Received message with key {0} and message {1}", key, message)); //$NON-NLS-1$
	}
}