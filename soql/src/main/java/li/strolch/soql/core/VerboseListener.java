package li.strolch.soql.core;

import java.util.Collections;
import java.util.List;

import org.antlr.v4.runtime.*;

/**
 * @author msmock
 */
public class VerboseListener extends BaseErrorListener implements ANTLRErrorListener {

	@Override
	public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine,
			String msg, RecognitionException e) {

		List<String> stack = ((Parser) recognizer).getRuleInvocationStack();
		Collections.reverse(stack);
		System.err.println("rule stack: " + stack);

		String text = "line " + line + ":" + charPositionInLine + " at " + offendingSymbol + ": " + msg;
		System.err.println(text);

		throw new SOQLParseException(text);

	}

}
