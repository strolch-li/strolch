package ch.eitchnet.utils.io;

/**
 * <p>
 * This interface defines an API for use in situations where long running jobs notify observers of the jobs status. The
 * jobs has a size which is a primitive long value e.g. the number of bytes parsed/ to be parsed in a file
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
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
	 *            {@link #begin(long)} but in case of errors it can be different
	 */
	public void end(int percent, long position);
}
