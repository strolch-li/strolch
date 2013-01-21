/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.xmlpers
 *
 * ch.eitchnet.java.xmlpers is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.xmlpers is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.xmlpers.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.xmlpers;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.utils.helper.XmlHelper;
import ch.eitchnet.utils.objectfilter.ITransactionObject;
import ch.eitchnet.utils.objectfilter.ObjectFilter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceTransaction {

	private static final Logger logger = LoggerFactory.getLogger(XmlPersistenceTransaction.class);

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
		if (this.docBuilder == null) {
			try {
				this.docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			} catch (ParserConfigurationException e) {
				throw new XmlPersistenceExecption("Failed to load document builder: " + e.getLocalizedMessage(), e);
			}
		}
		return this.docBuilder;
	}

	/**
	 * @return
	 */
	protected DOMImplementation getDomImpl() {
		if (this.domImplementation == null)
			this.domImplementation = getDocBuilder().getDOMImplementation();
		return this.domImplementation;
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
		XmlDao<Object> dao = this.xmlDaoFactory.getDao(type);

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

		XmlDao<Object> dao = this.xmlDaoFactory.getDao(type);

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

		if (this.verbose)
			XmlPersistenceTransaction.logger.info("Committing...");

		Set<String> keySet = this.objectFilter.keySet();
		if (keySet.isEmpty())
			return;

		for (String key : keySet) {

			XmlDao<Object> dao = this.xmlDaoFactory.getDao(key);

			List<ITransactionObject> removed = this.objectFilter.getRemoved(key);
			if (removed.isEmpty()) {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info("No objects removed in this tx.");
			} else {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info(removed.size() + " objects removed in this tx.");

				for (ITransactionObject object : removed) {

					String type = dao.getType(object);
					String subType = dao.getSubType(object);
					String id = dao.getId(object);

					this.persister.remove(type, subType, id);
				}
			}

			List<ITransactionObject> updated = this.objectFilter.getUpdated(key);
			if (updated.isEmpty()) {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info("No objects updated in this tx.");
			} else {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info(updated.size() + " objects updated in this tx.");

				for (ITransactionObject object : updated) {

					String type = dao.getType(object);
					String subType = dao.getSubType(object);
					String id = dao.getId(object);

					Document doc = XmlHelper.createDocument();
					Element asDom = dao.serializeToDom(object, doc);
					doc.appendChild(asDom);
					this.persister.saveOrUpdate(type, subType, id, doc);
				}
			}

			List<ITransactionObject> added = this.objectFilter.getAdded(key);
			if (added.isEmpty()) {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info("No objects added in this tx.");
			} else {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info(added.size() + " objects added in this tx.");

				for (ITransactionObject object : added) {

					String type = dao.getType(object);
					String subType = dao.getSubType(object);
					String id = dao.getId(object);

					Document doc = XmlHelper.createDocument();
					Element asDom = dao.serializeToDom(object, doc);
					doc.appendChild(asDom);
					this.persister.saveOrUpdate(type, subType, id, doc);
				}
			}
		}

		this.objectFilter.clearCache();
		XmlPersistenceTransaction.logger.info("Completed TX");
	}
}
