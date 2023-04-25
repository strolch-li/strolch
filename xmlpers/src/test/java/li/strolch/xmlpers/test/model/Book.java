/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.xmlpers.test.model;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
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
			throw new IllegalArgumentException("Id may not be null!");
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
	 * 		the title to set
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
	 * 		the author to set
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
	 * 		the press to set
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
	 * 		the price to set
	 */
	public void setPrice(double price) {
		this.price = price;
	}
}
