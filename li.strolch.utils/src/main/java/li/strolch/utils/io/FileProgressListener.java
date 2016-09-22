/*
 * Copyright 2014 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.utils.io;

/**
 * <p>
 * This interface defines an API for use in situations where long running jobs notify observers of the jobs status. The
 * jobs has a size which is a primitive long value e.g. the number of bytes parsed/ to be parsed in a file
 * </p>
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public interface FileProgressListener {

	/**
	 * Notify the listener that the progress has begun
	 * 
	 * @param percent
	 *            percent completed
	 * @param position
	 *            the position relative to the job size
	 * @param size
	 *            the size of the job which is to be accomplished
	 */
	public void begin(int percent, long position, long size);

	/**
	 * Notifies the listener of incremental progress
	 * 
	 * @param percent
	 *            percent completed
	 * @param position
	 *            the position relative to the job size
	 */
	public void progress(int percent, long position);

	/**
	 * Notifies the listener that the progress is completed
	 * 
	 * @param percent
	 *            the percent completed. Ideally the value would be 100, but in cases of errors it can be less
	 * @param position
	 *            the position where the job finished. Ideally the value would be the same as the size given at
	 *            {@link #begin(int, long, long)} but in case of errors it can be different
	 */
	public void end(int percent, long position);
}
