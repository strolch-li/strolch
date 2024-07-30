package li.strolch.utils.helper;

import java.io.File;
import java.io.IOException;
import java.net.*;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.nio.file.Files.readAllLines;
import static java.nio.file.Files.readString;
import static java.util.Spliterator.ORDERED;
import static java.util.Spliterators.spliteratorUnknownSize;

public class NetworkHelper {

	public static List<Inet4Address> findInet4Addresses() throws SocketException {

		List<Inet4Address> inet4Addresses = new ArrayList<>();

		Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
		while (interfaces.hasMoreElements()) {
			NetworkInterface ni = interfaces.nextElement();

			if (ni.isLoopback() || ni.isPointToPoint() || ni.isVirtual())
				continue;

			Enumeration<InetAddress> inetAddresses = ni.getInetAddresses();
			while (inetAddresses.hasMoreElements()) {
				InetAddress ia = inetAddresses.nextElement();

				if (ia instanceof Inet4Address)
					inet4Addresses.add((Inet4Address) ia);
			}
		}

		return inet4Addresses;
	}

	public static String formatMacAddress(NetworkInterface networkInterface) {
		try {
			return formatMacAddress(networkInterface.getHardwareAddress());
		} catch (SocketException e) {
			throw new IllegalStateException(
					"Failed to get hardware address for network interface " + networkInterface.getDisplayName(), e);
		}
	}

	public static String formatMacAddress(byte[] bytes) {
		StringBuilder sb = new StringBuilder(17);
		for (byte b : bytes) {
			if (!sb.isEmpty())
				sb.append(':');
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
	}

	public static void main(String[] args) throws IOException {

		Stream<StrolchNetworkInterface> interfaceStream = streamStrolchNetworkInterface();

		interfaceStream.forEach(t -> System.out.printf("%20s: %s%n", t.name(), t.hwAddress()));
	}

	public static Stream<StrolchNetworkInterface> streamStrolchNetworkInterface() {

		if (!System.getProperty("os.name").equals("Linux")) {
			Enumeration<NetworkInterface> interfaces = null;
			try {
				interfaces = NetworkInterface.getNetworkInterfaces();
			} catch (SocketException e) {
				throw new IllegalStateException("Failed to read network interfaces ", e);
			}
			return StreamSupport.stream(spliteratorUnknownSize(interfaces.asIterator(), ORDERED), false).map(ni -> {
				try {
					List<InetAddress> addresses = Collections.list(ni.getInetAddresses());
					if (addresses.isEmpty())
						addresses = List.of(Inet4Address.getByName("0.0.0.0"));
					return new StrolchNetworkInterface(ni.getName(), formatMacAddress(ni), addresses);
				} catch (UnknownHostException e) {
					throw new IllegalStateException("Failed to read information for device " + ni.getName(), e);
				}
			});
		}

		// Read all available device names
		Pattern pattern = Pattern.compile("^ *(.*):");
		List<String> kernelInterfaceLines;
		try {
			kernelInterfaceLines = readAllLines(Path.of("/proc/net/dev"));
		} catch (IOException e) {
			throw new IllegalStateException("Failed to read /proc/net/dev", e);
		}

		return kernelInterfaceLines.stream().map(line -> {
			Matcher m = pattern.matcher(line);
			if (m.find())
				return m.group(1).trim();
			return null;
		}).filter(Objects::nonNull).filter(device -> {
			if (device.startsWith("br-"))
				return false;
			if (device.startsWith("veth"))
				return false;
			if (device.startsWith("docker"))
				return false;
			return true;
		}).map(device -> {
			try {
				String hwAddress = readString(Path.of("/sys/class/net/" + device + "/address")).trim();
				File operStateFile = new File("/sys/class/net/" + device + "/operstate");
				File carrierFile = new File("/sys/class/net/" + device + "/carrier");
				boolean hasNoCarrier;
				if (operStateFile.exists()) {
					hasNoCarrier = operStateFile.exists() && readString(operStateFile.toPath()).trim().equals("down");
				} else {
					hasNoCarrier = readString(carrierFile.toPath()).trim().equals("0");
				}
				if (hasNoCarrier)
					return new StrolchNetworkInterface(device, hwAddress, List.of(Inet4Address.getByName("0.0.0.0")));

				NetworkInterface ni = NetworkInterface.getByName(device);
				if (ni.isLoopback() || ni.isPointToPoint() || ni.isVirtual())
					return null;

				List<InetAddress> addresses = Collections.list(ni.getInetAddresses());
				if (addresses.isEmpty())
					addresses = List.of(Inet4Address.getByName("0.0.0.0"));
				return new StrolchNetworkInterface(ni.getName(), hwAddress, addresses);

			} catch (IOException e) {
				throw new IllegalStateException("Failed to read information for device " + device, e);
			}
		}).filter(Objects::nonNull);
	}
}
