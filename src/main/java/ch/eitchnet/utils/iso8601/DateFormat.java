/*
 * Copyright (c) 2006 - 2011 Apixxo AG Hauptgasse 25 4600
 * Olten
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.utils.iso8601;

/**
 * interface for all date formats internally used by rsp applications
 * 
 * @author msmock
 */
public interface DateFormat {

	/**
	 * format a long to string
	 * 
	 * @param l
	 * @return the formatted string of the long value
	 */
	public String format(long l);

	/**
	 * parse a string to long
	 * 
	 * @param s
	 * @return the value parsed
	 */
	public long parse(String s);

}
