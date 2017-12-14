	import java.io.InputStream;
	import java.io.OutputStream;
	import java.net.ServerSocket;
	import java.net.Socket;

	public class Server {

		public static void main(String[] args) throws Exception {

			ServerSocket s = new ServerSocket(33000);
			System.out.println("Waiting for client");
			Socket c = s.accept();
			System.out.println("Client connected");

			InputStream in;
			OutputStream out;
			try {
				in = c.getInputStream();
				out = c.getOutputStream();
			} catch (Exception e) {
				System.err.print("Cannot use client socket: " + e);
				Thread.currentThread().interrupt();
				return;
			}

			while (true) {
				String msg = "Hello World!";
				System.out.println("Sending " + msg);
				out.write(msg.length());
				out.write(msg.getBytes());
				out.flush();

				int length = in.read();
				byte[] buffer = new byte[length];
				int read = in.read(buffer);
				if (read != length)
					throw new IllegalStateException("Expected " + length + " bytes, but read " + read);

				String line = new String(buffer, "UTF-8");
				System.out.println("Received " + line.trim());

				System.out.println("Working...");
				Thread.sleep(2000L);
			}
		}
	}
