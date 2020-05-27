package li.strolch.persistence.xml.model;

import li.strolch.model.log.LogMessage;
import li.strolch.model.Tags;
import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceContextFactory;
import li.strolch.xmlpers.objref.IdOfSubTypeRef;
import li.strolch.xmlpers.objref.ObjectRef;
import li.strolch.xmlpers.objref.ObjectReferenceCache;

public class LogMessageContextFactory implements PersistenceContextFactory<LogMessage> {
	@Override
	public PersistenceContext<LogMessage> createCtx(ObjectRef objectRef) {
		PersistenceContext<LogMessage> ctx = new PersistenceContext<>(objectRef);
		ctx.setParserFactory(new LogMessageParserFactory());
		return ctx;
	}

	@Override
	public PersistenceContext<LogMessage> createCtx(ObjectReferenceCache objectRefCache, LogMessage logMessage) {
		IdOfSubTypeRef objectRef = objectRefCache
				.getIdOfSubTypeRef(Tags.LOG_MESSAGE, logMessage.getRealm(), logMessage.getId());
		PersistenceContext<LogMessage> ctx = createCtx(objectRef);
		ctx.setObject(logMessage);
		return ctx;
	}
}
