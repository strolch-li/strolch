import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

public class Client {

	public static void main(String[] args) throws Exception {

		try (Socket s = new Socket("localhost", 33000)) {

			InputStream in = s.getInputStream();
			OutputStream out = s.getOutputStream();

			while (true) {

				int length = in.read();
				byte[] buffer = new byte[length];
				int read = in.read(buffer);
				if (read != length)
					throw new IllegalStateException("Expected " + length + " bytes, but read " + read);

				String line = new String(buffer, "UTF-8");
				System.out.println("Received " + line.trim());

				String msg = "Welcome!";
				System.out.println("Sending: " + msg);
				out.write(msg.length());
				out.write(msg.getBytes());
				out.flush();
			}
		}
	}
}
