/*
 * Copyright (c) 2006 - 2011 Apixxo AG Hauptgasse 25 4600
 * Olten
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.utils.iso8601;

/**
 * interface for the worktime format
 * 
 * @author msmock
 */
public interface WorktimeFormat {

	/**
	 * format a long to string
	 * 
	 * @param l
	 * @return formatted string if the long argument
	 */
	public String format(long l);

	/**
	 * parse a string to long
	 * 
	 * @param s
	 * @return the long value parsed
	 */
	public long parse(String s);

}
