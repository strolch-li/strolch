package ch.eitchnet.communication.chat;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.MessageFormat;

import ch.eitchnet.utils.helper.StringHelper;

public class Chat {

	public static void main(String[] args) {

		if (args.length < 3)
			printIllegalArgsAndExit(args);

		if (args[0].equals("server")) { //$NON-NLS-1$
			if (args.length != 3)
				printIllegalArgsAndExit(args);
			startServer(args);
		} else if (args[0].equals("client")) { //$NON-NLS-1$
			if (args.length != 4)
				printIllegalArgsAndExit(args);
			startClient(args);
		}
	}

	private static void startServer(String[] args) {

		// port
		int port;
		String portS = args[1];
		try {
			port = Integer.parseInt(portS);
		} catch (NumberFormatException e) {
			System.err.println(MessageFormat.format("Illegal port: {0}", portS)); //$NON-NLS-1$
			printIllegalArgsAndExit(args);
			return;
		}
		if (port < 1 || port > 65535) {
			System.err.println(MessageFormat.format("Illegal port: {0}", port)); //$NON-NLS-1$
			printIllegalArgsAndExit(args);
			return;
		}

		// username
		String username = args[2];

		// start
		ChatServer chatServer = new ChatServer(port, username);
		chatServer.start();
	}

	private static void startClient(String[] args) {

		// server
		InetAddress host;
		String hostS = args[1];
		try {
			host = InetAddress.getByName(hostS);
		} catch (UnknownHostException e1) {
			System.err.println(MessageFormat.format("Illegal server address: {0}", hostS)); //$NON-NLS-1$
			printIllegalArgsAndExit(args);
			return;
		}

		// port
		int port;
		String portS = args[2];
		try {
			port = Integer.parseInt(portS);
		} catch (NumberFormatException e) {
			System.err.println(MessageFormat.format("Illegal port: {0}", portS)); //$NON-NLS-1$
			printIllegalArgsAndExit(args);
			return;
		}
		if (port < 1 || port > 65535) {
			System.err.println(MessageFormat.format("Illegal port: {0}", port)); //$NON-NLS-1$
			printIllegalArgsAndExit(args);
			return;
		}

		// username
		String username = args[3];

		// start
		ChatClient chatClient = new ChatClient(host, port, username);
		chatClient.start();
	}

	private static void printIllegalArgsAndExit(String[] args) {
		System.err.print("Illegal arguments: "); //$NON-NLS-1$
		if (args.length == 0) {
			System.err.print("(none)"); //$NON-NLS-1$
		} else {
			for (String arg : args) {
				System.err.print(arg + StringHelper.SPACE);
			}
		}
		System.err.println();
		System.err.println("Usage: java ...Chat server <port> <username>"); //$NON-NLS-1$
		System.err.println("Usage: java ...Chat client <server> <port> <username>"); //$NON-NLS-1$
		System.exit(1);
	}
}
