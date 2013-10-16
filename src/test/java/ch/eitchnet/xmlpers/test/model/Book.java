/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.xmlpers.test.model;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class Book {

	private final Long id;
	private String title;
	private String author;
	private String press;
	private double price;

	/**
	 * @param id
	 * @param title
	 * @param author
	 * @param press
	 * @param price
	 */
	public Book(Long id, String title, String author, String press, double price) {
		super();
		this.id = id;
		this.title = title;
		this.author = author;
		this.press = press;
		this.price = price;
	}

	/**
	 * 
	 */
	public Book(Long id) {
		if (id == null)
			throw new IllegalArgumentException("Id may not be null!"); //$NON-NLS-1$
		this.id = id;
	}

	/**
	 * @return the id
	 */
	public Long getId() {
		return this.id;
	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return this.title;
	}

	/**
	 * @param title
	 *            the title to set
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * @return the author
	 */
	public String getAuthor() {
		return this.author;
	}

	/**
	 * @param author
	 *            the author to set
	 */
	public void setAuthor(String author) {
		this.author = author;
	}

	/**
	 * @return the press
	 */
	public String getPress() {
		return this.press;
	}

	/**
	 * @param press
	 *            the press to set
	 */
	public void setPress(String press) {
		this.press = press;
	}

	/**
	 * @return the price
	 */
	public double getPrice() {
		return this.price;
	}

	/**
	 * @param price
	 *            the price to set
	 */
	public void setPrice(double price) {
		this.price = price;
	}
}
