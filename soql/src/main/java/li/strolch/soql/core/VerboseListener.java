package li.strolch.soql.core;

import java.util.Collections;
import java.util.List;

import org.antlr.v4.runtime.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author msmock
 */
public class VerboseListener extends BaseErrorListener implements ANTLRErrorListener {

	private static final Logger logger = LoggerFactory.getLogger(VerboseListener.class);

	@Override
	public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine,
			String msg, RecognitionException e) {

		List<String> stack = ((Parser) recognizer).getRuleInvocationStack();
		Collections.reverse(stack);
		logger.error("rule stack: " + stack);

		String text = "line " + line + ":" + charPositionInLine + " at " + offendingSymbol + ": " + msg;
		logger.error(text);

		throw new SOQLParseException(text);
	}
}
