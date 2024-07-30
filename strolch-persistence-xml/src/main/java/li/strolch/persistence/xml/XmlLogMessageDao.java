package li.strolch.persistence.xml;

import java.util.Collection;
import java.util.List;

import li.strolch.model.log.LogMessage;
import li.strolch.model.Tags;
import li.strolch.persistence.api.LogMessageDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.objref.SubTypeRef;

public class XmlLogMessageDao implements LogMessageDao {

	private final PersistenceTransaction tx;

	public XmlLogMessageDao(StrolchTransaction tx) {
		XmlStrolchTransaction strolchTx = (XmlStrolchTransaction) tx;
		this.tx = strolchTx.getTx();
	}

	protected String getClassType() {
		return Tags.LOG_MESSAGE;
	}

	@Override
	public List<LogMessage> queryLatest(String realm, int maxNr) {
		SubTypeRef subTypeRef = this.tx.getManager().getObjectRefCache().getSubTypeRef(getClassType(), realm);
		return this.tx.getObjectDao().queryAll(subTypeRef, true, file -> true, maxNr);
	}

	@Override
	public void save(LogMessage logMessage) {
		this.tx.getObjectDao().add(logMessage);
	}

	@Override
	public void saveAll(List<LogMessage> logMessages) {
		this.tx.getObjectDao().addAll(logMessages);
	}

	@Override
	public void updateState(LogMessage logMessage) {
		this.tx.getObjectDao().update(logMessage);
	}

	@Override
	public void updateStates(Collection<LogMessage> logMessages) {
		logMessages.forEach(logMessage -> this.tx.getObjectDao().update(logMessage));
	}

	@Override
	public void remove(LogMessage logMessage) {
		this.tx.getObjectDao().remove(logMessage);
	}

	@Override
	public void removeAll(List<LogMessage> logMessages) {
		this.tx.getObjectDao().removeAll(logMessages);
	}
}
