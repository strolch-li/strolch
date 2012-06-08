/*
 * Copyright (c) 2010 - 2011
 * 
 * Apixxo AG 
 * Hauptgasse 25
 * 4600 Olten
 * 
 * All rights reserved.
 * 
 */
package ch.eitchnet.xmlpers;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.log4j.Logger;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.utils.objectfilter.ITransactionObject;
import ch.eitchnet.utils.objectfilter.ObjectFilter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceTransaction {

	private static final Logger logger = Logger.getLogger(XmlPersistenceTransaction.class);

	private boolean verbose;
	private XmlFilePersister persister;
	private XmlDaoFactory xmlDaoFactory;
	private ObjectFilter<ITransactionObject> objectFilter;
	private DocumentBuilder docBuilder;
	private DOMImplementation domImplementation;

	/**
	 * @param persister
	 * @param xmlDaoFactory
	 * @param objectFilter
	 */
	public void initialize(XmlFilePersister persister, XmlDaoFactory xmlDaoFactory,
			ObjectFilter<ITransactionObject> objectFilter, boolean verbose) {
		this.persister = persister;
		this.xmlDaoFactory = xmlDaoFactory;
		this.objectFilter = objectFilter;
		this.verbose = verbose;
	}

	private DocumentBuilder getDocBuilder() {
		if (docBuilder == null) {
			try {
				docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			} catch (ParserConfigurationException e) {
				throw new XmlPersistenceExecption("Failed to load document builder: " + e.getLocalizedMessage(), e);
			}
		}
		return docBuilder;
	}

	/**
	 * @return
	 */
	protected DOMImplementation getDomImpl() {
		if (domImplementation == null)
			domImplementation = getDocBuilder().getDOMImplementation();
		return domImplementation;
	}

	/*
	 * modifying methods
	 */

	/**
	 * @param object
	 */
	public void add(ITransactionObject object) {
		this.objectFilter.add(object);
	}

	/**
	 * @param objects
	 */
	public void addAll(List<ITransactionObject> objects) {
		this.objectFilter.addAll(objects);
	}

	/**
	 * @param object
	 */
	public void update(ITransactionObject object) {
		this.objectFilter.update(object);
	}

	/**
	 * @param objects
	 */
	public void updateAll(List<ITransactionObject> objects) {
		this.objectFilter.updateAll(objects);
	}

	/**
	 * @param object
	 */
	public void remove(ITransactionObject object) {
		this.objectFilter.remove(object);
	}

	/**
	 * @param objects
	 */
	public void removeAll(List<ITransactionObject> objects) {
		this.objectFilter.removeAll(objects);
	}

	/*
	 * querying methods
	 */

	/**
	 * @param type
	 * @return
	 */
	public Set<String> queryKeySet(String type) {
		return queryKeySet(type, null);
	}

	/**
	 * @param type
	 * @param subType
	 * @return
	 */
	public Set<String> queryKeySet(String type, String subType) {
		return this.persister.queryKeySet(type, subType);
	}

	/**
	 * @param type
	 * @return
	 */
	public long querySize(String type) {
		return querySize(type, null);
	}

	/**
	 * @param type
	 * @param subType
	 * @return
	 */
	public long querySize(String type, String subType) {
		return this.persister.querySize(type, subType);
	}

	/**
	 * @param type
	 * @return
	 */
	public <T> List<T> queryAll(String type) {
		return queryAll(type, null);
	}

	/**
	 * @param type
	 * @param subType
	 * @return
	 */
	public <T> List<T> queryAll(String type, String subType) {

		// XXX ok, this is very ugly, but for starters it will have to do
		XmlDao<Object> dao = xmlDaoFactory.getDao(type);

		List<Element> elements = this.persister.queryAll(type, subType, getDocBuilder());
		List<T> objects = new ArrayList<T>(elements.size());

		for (Element element : elements) {
			@SuppressWarnings("unchecked")
			T object = (T) dao.parseFromDom(element);
			objects.add(object);
		}

		return objects;
	}

	/**
	 * @param type
	 * @param id
	 * @return
	 */
	public <T> T queryById(String type, String id) {
		return queryById(type, null, id);
	}

	/**
	 * @param type
	 * @param subType
	 * @param id
	 * @return
	 */
	public <T> T queryById(String type, String subType, String id) {

		XmlDao<Object> dao = xmlDaoFactory.getDao(type);

		Element element = this.persister.queryById(type, subType, id, getDocBuilder());
		if (element == null)
			throw new XmlPersistenceExecption("No object exists for " + type + " / " + subType + " / " + id);

		@SuppressWarnings("unchecked")
		T object = (T) dao.parseFromDom(element);

		return object;
	}

	/*
	 * committing
	 */

	/**
	 * 
	 */
	void commitTx() {

		if (verbose)
			logger.info("Committing...");

		Set<String> keySet = objectFilter.keySet();
		if (keySet.isEmpty())
			return;

		for (String key : keySet) {

			XmlDao<Object> dao = xmlDaoFactory.getDao(key);

			List<ITransactionObject> removed = objectFilter.getRemoved(key);
			if (removed.isEmpty()) {
				if (verbose)
					logger.info("No objects removed in this tx.");
			} else {
				if (verbose)
					logger.info(removed.size() + " objects removed in this tx.");

				for (ITransactionObject object : removed) {

					String type = dao.getType(object);
					String subType = dao.getSubType(object);
					String id = dao.getId(object);

					persister.remove(type, subType, id);
				}
			}

			List<ITransactionObject> updated = objectFilter.getUpdated(key);
			if (updated.isEmpty()) {
				if (verbose)
					logger.info("No objects updated in this tx.");
			} else {
				if (verbose)
					logger.info(updated.size() + " objects updated in this tx.");

				for (ITransactionObject object : updated) {

					String type = dao.getType(object);
					String subType = dao.getSubType(object);
					String id = dao.getId(object);

					Document asDom = dao.serializeToDom(object, getDomImpl());
					persister.saveOrUpdate(type, subType, id, asDom);
				}
			}

			List<ITransactionObject> added = objectFilter.getAdded(key);
			if (added.isEmpty()) {
				if (verbose)
					logger.info("No objects added in this tx.");
			} else {
				if (verbose)
					logger.info(updated.size() + " objects added in this tx.");

				for (ITransactionObject object : added) {

					String type = dao.getType(object);
					String subType = dao.getSubType(object);
					String id = dao.getId(object);

					Document asDom = dao.serializeToDom(object, getDomImpl());
					persister.saveOrUpdate(type, subType, id, asDom);
				}
			}
		}

		objectFilter.clearCache();
		logger.info("Completed TX");
	}
}
