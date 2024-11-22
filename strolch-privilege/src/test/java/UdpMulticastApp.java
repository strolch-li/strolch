import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.*;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class UdpMulticastApp {

	private static final Logger logger = LoggerFactory.getLogger(UdpMulticastApp.class);

	private static final String MULTICAST_GROUP = "224.0.0.1";
	private static final int PORT = 5000;
	private static final int BUFFER_SIZE = 1024;
	private static final String ID = UUID.randomUUID().toString(); // Unique ID for this instance
	private static volatile String controllerId = null;
	private static final Map<String, Instant> heartbeatMap = new HashMap<>();
	private static final int HEARTBEAT_TIMEOUT_MS = 5000; // 5 seconds

	public static void main(String[] args) {
		Thread receiverThread = new Thread(UdpMulticastApp::receiveMessages);
		Thread senderThread = new Thread(UdpMulticastApp::sendMessages);
		Thread healthCheckThread = new Thread(UdpMulticastApp::checkHealth);

		receiverThread.start();
		senderThread.start();
		healthCheckThread.start();
	}

	private static void sendMessages() {
		try (MulticastSocket socket = new MulticastSocket()) {
			InetAddress group = InetAddress.getByName(MULTICAST_GROUP);

			while (true) {
				JsonObject message = new JsonObject();
				message.addProperty("type", "heartbeat");
				message.addProperty("id", ID);

				byte[] buffer = message.toString().getBytes();
				DatagramPacket packet = new DatagramPacket(buffer, buffer.length, group, PORT);
				socket.send(packet);

				System.out.println("Sent: " + message);
				Thread.sleep(2000); // Send every 2 seconds
			}
		} catch (IOException | InterruptedException e) {
			logger.error(e.getMessage(), e);
		}
	}

	private static void receiveMessages() {
		try (MulticastSocket socket = new MulticastSocket(PORT)) {
			InetAddress group = InetAddress.getByName(MULTICAST_GROUP);
			NetworkInterface networkInterface = NetworkInterface.getByInetAddress(InetAddress.getLocalHost());
			socket.joinGroup(new InetSocketAddress(group, PORT), networkInterface);

			byte[] buffer = new byte[BUFFER_SIZE];

			while (true) {
				DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
				socket.receive(packet);

				String receivedData = new String(packet.getData(), 0, packet.getLength());
				System.out.println("Received: " + receivedData);

				JsonObject message = JsonParser.parseString(receivedData).getAsJsonObject();
				handleMessage(message);
			}
		} catch (IOException e) {
			logger.error(e.getMessage(), e);
		}
	}

	private static synchronized void handleMessage(JsonObject message) {
		String type = message.get("type").getAsString();
		String senderId = message.get("id").getAsString();

		switch (type) {
			case "heartbeat":
				// Update last heartbeat time
				heartbeatMap.put(senderId, Instant.now());
				if (controllerId == null || senderId.compareTo(controllerId) < 0) {
					electController(senderId);
				}
				break;

			default:
				System.out.println("Unknown message type: " + type);
		}
	}

	private static synchronized void electController(String receivedId) {
		if (controllerId == null || receivedId.compareTo(controllerId) < 0) {
			controllerId = receivedId;
			System.out.println("New Controller Elected: " + controllerId);
		}
	}

	private static void checkHealth() {
		while (true) {
			try {
				Thread.sleep(2000); // Check every 2 seconds

				Instant now = Instant.now();
				heartbeatMap.entrySet().removeIf(entry -> {
					boolean expired = now.toEpochMilli() - entry.getValue().toEpochMilli() > HEARTBEAT_TIMEOUT_MS;
					if (expired) {
						System.out.println("Heartbeat missed from: " + entry.getKey());
					}
					return expired;
				});

				// If the current controller is missing, elect a new one
				if (controllerId != null && (
						!heartbeatMap.containsKey(controllerId)
								|| now.toEpochMilli() - heartbeatMap.get(controllerId).toEpochMilli()
								> HEARTBEAT_TIMEOUT_MS)) {
					System.out.println("Controller missed heartbeat. Re-electing...");
					controllerId = heartbeatMap.keySet().stream().min(String::compareTo).orElse(null);
					if (controllerId != null) {
						System.out.println("New Controller Elected: " + controllerId);
					} else {
						System.out.println("No applications available for election.");
					}
				}
			} catch (InterruptedException e) {
				logger.error(e.getMessage(), e);
			}
		}
	}
}
