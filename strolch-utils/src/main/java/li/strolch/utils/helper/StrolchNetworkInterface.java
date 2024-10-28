package li.strolch.utils.helper;

import java.net.InetAddress;
import java.util.List;

public record StrolchNetworkInterface(String name, String hwAddress, List<InetAddress> inetAddresses) {
	public StrolchNetworkInterface(String name, String hwAddress, List<InetAddress> inetAddresses) {
		this.name = name;
		this.hwAddress = hwAddress;
		this.inetAddresses = List.copyOf(inetAddresses);
	}
}
