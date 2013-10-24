/*
 * Copyright (c) 2006 - 2011 Apixxo AG Hauptgasse 25 4600
 * Olten
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.utils.iso8601;

/**
 * interface for all duration formats internally used by the platform
 * 
 * @author msmock
 */
public interface DurationFormat {

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
