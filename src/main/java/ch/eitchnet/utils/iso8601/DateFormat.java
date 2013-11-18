/*
 * Copyright (c) 2006 - 2011 Apixxo AG Hauptgasse 25 4600
 * Olten
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.utils.iso8601;

import java.util.Date;

/**
 * interface for all date formats internally used by rsp applications
 * 
 * @author msmock
 */
public interface DateFormat {

	/**
	 * format a long to string
	 * 
	 * @param timepoint
	 * @return the formatted string of the long value
	 */
	public String format(long timepoint);

	/**
	 * format a Date to string
	 * 
	 * @param date
	 * @return the formatted string of the long value
	 */
	public String format(Date date);

	/**
	 * parse a string to long
	 * 
	 * @param s
	 * @return the value parsed
	 */
	public long parseLong(String s);

	/**
	 * parse a string to Date
	 * 
	 * @param s
	 * @return the value parsed
	 */
	public Date parse(String s);
}
