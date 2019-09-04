package li.strolch.utils;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;

public class RemoveCRFilterWriter extends FilterWriter {

	public RemoveCRFilterWriter(Writer wrappedWriter) {
		super(wrappedWriter);
	}

	@Override
	public void write(int c) throws IOException {
		if (c != (int) ('\r')) {
			super.write(c);
		}
	}

	@Override
	public void write(char[] cbuf, int offset, int length) throws IOException {
		int localOffset = offset;
		for (int i = localOffset; i < offset + length; ++i) {
			if (cbuf[i] == '\r') {
				if (i > localOffset) {
					super.write(cbuf, localOffset, i - localOffset);
				}
				localOffset = i + 1;
			}
		}
		if (localOffset < offset + length) {
			super.write(cbuf, localOffset, offset + length - localOffset);
		}
	}

	@Override
	public void write(String str, int offset, int length) throws IOException {
		int localOffset = offset;
		for (int i = localOffset; i < offset + length; ++i) {
			if (str.charAt(i) == '\r') {
				if (i > localOffset) {
					super.write(str, localOffset, i - localOffset);
				}
				localOffset = i + 1;
			}
		}
		if (localOffset < offset + length) {
			super.write(str, localOffset, offset + length - localOffset);
		}
	}
}