package li.strolch.utils.helper;

import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

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

	public static String formatMacAddress(byte[] bytes) {
		StringBuilder sb = new StringBuilder(17);
		for (byte b : bytes) {
			if (!sb.isEmpty())
				sb.append(':');
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
	}
}
