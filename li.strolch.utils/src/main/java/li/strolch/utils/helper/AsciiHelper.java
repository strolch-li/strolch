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
package li.strolch.utils.helper;

/**
 * ASCII constants
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class AsciiHelper {

	/**
	 * ASCII Value 0, interpretation: NUL <br> Description: Null character
	 */
	public static final char NUL = (char) 0; // Null character

	/**
	 * ASCII Value 1, interpretation: SOH<br> Description: Start of Header
	 */
	public static final char SOH = (char) 1; // Start of Header

	/**
	 * ASCII Value 2, interpretation: STX<br> Description: Start of Text
	 */
	public static final char STX = (char) 2; // Start of Text

	/**
	 * ASCII Value 3, interpretation: ETX<br> Description: End of Text
	 */
	public static final char ETX = (char) 3; // End of Text

	/**
	 * ASCII Value 4, interpretation: EOT<br> Description: End of Transmission
	 */
	public static final char EOT = (char) 4; // End of Transmission

	/**
	 * ASCII Value 5, interpretation: ENQ<br> Description: Enquiry
	 */
	public static final char ENQ = (char) 5; // Enquiry

	/**
	 * ASCII Value 6, interpretation: ACK<br> Description: Acknowledgement
	 */
	public static final char ACK = (char) 6; // Acknowledgement

	/**
	 * ASCII Value 7, interpretation: BEL<br> Description: Bell
	 */
	public static final char BEL = (char) 7; // Bell

	/**
	 * ASCII Value 8, interpretation: BS<br> Description: Backspace
	 */
	public static final char BS = (char) 8; // Backspace

	/**
	 * ASCII Value 9, interpretation: HT<br> Description: Horizontal Tab
	 */
	public static final char HT = (char) 9; // Horizontal Tab

	/**
	 * ASCII Value 10, interpretation: LF<br> Description: Line Feed
	 */
	public static final char LF = (char) 10; // Line Feed

	/**
	 * ASCII Value 11, interpretation: VT<br> Description: Vertical Tab
	 */
	public static final char VT = (char) 11; // Vertical Tab

	/**
	 * ASCII Value 12, interpretation: FF<br> Description: Form Feed
	 */
	public static final char FF = (char) 12; // Form Feed

	/**
	 * ASCII Value 13, interpretation: CR<br> Description: Carriage Return
	 */
	public static final char CR = (char) 13; // Carriage Return

	/**
	 * ASCII Value 14, interpretation: SO<br> Description: Shift Out
	 */
	public static final char SO = (char) 14; // Shift Out

	/**
	 * ASCII Value 15, interpretation: SI<br> Description: Shift In
	 */
	public static final char SI = (char) 15; // Shift In

	/**
	 * ASCII Value 16, interpretation: DLE<br> Description: Data Link Escape
	 */
	public static final char DLE = (char) 16; // Data Link Escape

	/**
	 * ASCII Value 17, interpretation: DC1<br> Description: (XON) Device Control 1
	 */
	public static final char DC1 = (char) 17; // (XON) Device Control 1

	/**
	 * ASCII Value 18, interpretation: DC2<br> Description: Device Control 2
	 */
	public static final char DC2 = (char) 18; // Device Control 2

	/**
	 * ASCII Value 19 interpretation: DC3<br> Description: (XOFF) Device Control 3
	 */
	public static final char DC3 = (char) 19; // (XOFF) Device Control 3

	/**
	 * ASCII Value 20, interpretation: DC4<br> Description: Device Control 4
	 */
	public static final char DC4 = (char) 20; // Device Control 4

	/**
	 * ASCII Value 21, interpretation: NAK<br> Description: Negative Acknowledgment
	 */
	public static final char NAK = (char) 21; // Negative Acknowledgment

	/**
	 * ASCII Value 22, interpretation: SYN<br> Description: Synchronous Idle
	 */
	public static final char SYN = (char) 22; // Synchronous Idle

	/**
	 * ASCII Value 23, interpretation: ETB<br> Description: End of Transmission Block
	 */
	public static final char ETB = (char) 23; // End of Transmission Block

	/**
	 * ASCII Value 24, interpretation: CAN<br> Description: Cancel
	 */
	public static final char CAN = (char) 24; // Cancel

	/**
	 * ASCII Value 25, interpretation: EM<br> Description: End of Medium
	 */
	public static final char EM = (char) 25; // End of Medium

	/**
	 * ASCII Value 26, interpretation: SUB<br> Description: Substitute
	 */
	public static final char SUB = (char) 26; // Substitute

	/**
	 * ASCII Value 27, interpretation: ESC<br> Description: Escape
	 */
	public static final char ESC = (char) 27; // Escape

	/**
	 * ASCII Value 28, interpretation: FS<br> Description: File Separator
	 */
	public static final char FS = (char) 28; // File Separator

	/**
	 * ASCII Value 29, interpretation: GS<br> Description: Group Separator
	 */
	public static final char GS = (char) 29; // Group Separator

	/**
	 * ASCII Value 30, interpretation: RS<br> Description: Request to Send (Record Separator)
	 */
	public static final char RS = (char) 30; // Request to Send (Record Separator)

	/**
	 * ASCII Value 31, interpretation: US<br> Description: Unit Separator
	 */
	public static final char US = (char) 31; // Unit Separator

	/**
	 * ASCII Value 32, interpretation: SP<br> Description: Space
	 */
	public static final char SP = (char) 32; // Space

	/**
	 * ASCII Value 127, interpretation: DEL<br> Description: Delete
	 */
	public static final char DEL = (char) 127; // Delete

	/**
	 * Returns the ASCII Text of a certain bye value
	 *
	 * @param b
	 * 		the byte
	 *
	 * @return String
	 */
	public static String getAsciiText(byte b) {
		return getAsciiText((char) b);
	}

	/**
	 * Returns the ASCII Text of a certain char value
	 *
	 * @param c
	 * 		the char
	 *
	 * @return String
	 */
	@SuppressWarnings("nls")
	public static String getAsciiText(char c) {
		// else if(c == ) { return "";}
		if (c == NUL) {
			return "NUL";
		} else if (c == SOH) {
			return "SOH";
		} else if (c == STX) {
			return "STX";
		} else if (c == ETX) {
			return "ETX";
		} else if (c == EOT) {
			return "EOT";
		} else if (c == ENQ) {
			return "ENQ";
		} else if (c == ACK) {
			return "ACK";
		} else if (c == BEL) {
			return "BEL";
		} else if (c == BS) {
			return "BS";
		} else if (c == HT) {
			return "HT";
		} else if (c == LF) {
			return "LF";
		} else if (c == VT) {
			return "VT";
		} else if (c == FF) {
			return "FF";
		} else if (c == CR) {
			return "CR";
		} else if (c == SO) {
			return "SO";
		} else if (c == SI) {
			return "SI";
		} else if (c == DLE) {
			return "DLE";
		} else if (c == DC1) {
			return "DC1";
		} else if (c == DC2) {
			return "DC2";
		} else if (c == DC3) {
			return "DC3";
		} else if (c == DC4) {
			return "DC4";
		} else if (c == NAK) {
			return "NAK";
		} else if (c == SYN) {
			return "SYN";
		} else if (c == ETB) {
			return "ETB";
		} else if (c == CAN) {
			return "CAN";
		} else if (c == EM) {
			return "EM";
		} else if (c == SUB) {
			return "SUB";
		} else if (c == ESC) {
			return "ESC";
		} else if (c == FS) {
			return "FS";
		} else if (c == GS) {
			return "GS";
		} else if (c == RS) {
			return "RS";
		} else if (c == US) {
			return "US";
		} else if (c == SP) {
			return "SP";
		} else if (c == DEL) {
			return "DEL";
		} else if ((c) > 32 && (c) < 127) {
			return String.valueOf(c);
		} else {
			return "(null:" + (byte) c + ")";
		}
	}
}
