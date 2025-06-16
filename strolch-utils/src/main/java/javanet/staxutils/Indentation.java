/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package javanet.staxutils;

/**
 * Characters that represent line breaks and indentation. These are represented as String-valued JavaBean properties.
 */
public interface Indentation {

	/**
	 * Two spaces; the default indentation.
	 */
	String DEFAULT_INDENT = "  ";

	/**
	 * Set the characters used for one level of indentation. The default is {@link #DEFAULT_INDENT}. "\t" is a popular
	 * alternative.
	 */
	void setIndent(String indent);

	/**
	 * The characters used for one level of indentation.
	 */
	String getIndent();

	/**
	 * "\n"; the normalized representation of end-of-line in <a href="http://www.w3.org/TR/xml11/#sec-line-ends">XML</a>.
	 */
	String NORMAL_END_OF_LINE = "\n";

	/**
	 * Set the characters that introduce a new line. The default is {@link #NORMAL_END_OF_LINE}. {@link
	 * IndentingXMLStreamWriter#getLineSeparator}() is a popular alternative.
	 */
	void setNewLine(String newLine);

	/**
	 * The characters that introduce a new line.
	 */
	String getNewLine();

}
