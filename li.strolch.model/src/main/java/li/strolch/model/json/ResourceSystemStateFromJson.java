package li.strolch.model.json;

import static li.strolch.model.StrolchValueType.DATE;
import static li.strolch.model.StrolchValueType.*;
import static li.strolch.model.Tags.Json.*;

import java.time.ZonedDateTime;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.model.Resource;
import li.strolch.model.StrolchValueType;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValue;

public class ResourceSystemStateFromJson {

	private long stateTime = System.currentTimeMillis();
	private boolean systemLoadAverageState;
	private boolean usableSpaceState;
	private boolean usedSpaceState;
	private boolean freeSpaceState;
	private boolean freePhysicalMemorySizeState;
	private boolean freeSwapSpaceSizeState;
	private boolean committedVirtualMemorySizeState;
	private boolean heapMemoryUsageInitState;
	private boolean heapMemoryUsageUsedState;
	private boolean heapMemoryUsageMaxState;
	private boolean heapMemoryUsageCommittedState;
	private boolean compactStates;

	public ResourceSystemStateFromJson compactStates() {
		this.compactStates = true;
		return this;
	}

	public ResourceSystemStateFromJson withStateTime(ZonedDateTime stateTime) {
		this.stateTime = stateTime.toInstant().toEpochMilli();
		return this;
	}

	public ResourceSystemStateFromJson withSystemLoadAverageState() {
		this.systemLoadAverageState = true;
		return this;
	}

	public ResourceSystemStateFromJson withUsableSpaceState() {
		this.usableSpaceState = true;
		return this;
	}

	public ResourceSystemStateFromJson withUsedSpaceState() {
		this.usedSpaceState = true;
		return this;
	}

	public ResourceSystemStateFromJson withFreeSpaceState() {
		this.freeSpaceState = true;
		return this;
	}

	public ResourceSystemStateFromJson withFreePhysicalMemorySizeState() {
		this.freePhysicalMemorySizeState = true;
		return this;
	}

	public ResourceSystemStateFromJson withFreeSwapSpaceSizeState() {
		this.freeSwapSpaceSizeState = true;
		return this;
	}

	public ResourceSystemStateFromJson withCommittedVirtualMemorySizeState() {
		this.committedVirtualMemorySizeState = true;
		return this;
	}

	public ResourceSystemStateFromJson withHeapMemoryUsageInitState() {
		this.heapMemoryUsageInitState = true;
		return this;
	}

	public ResourceSystemStateFromJson withHeapMemoryUsageUsedState() {
		this.heapMemoryUsageUsedState = true;
		return this;
	}

	public ResourceSystemStateFromJson withHeapMemoryUsageMaxState() {
		this.heapMemoryUsageMaxState = true;
		return this;
	}

	public ResourceSystemStateFromJson withHeapMemoryUsageCommittedState() {
		this.heapMemoryUsageCommittedState = true;
		return this;
	}

	public void fillElement(JsonObject systemStateJ, Resource resource) {

		if (systemStateJ.has(OPERATING_SYSTEM)) {
			JsonObject osJ = systemStateJ.get(OPERATING_SYSTEM).getAsJsonObject();

			String bagId = OPERATING_SYSTEM;
			String bagName = "Operating System";
			String bagType = "OperatingSystem";
			resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, OS_NAME, "OS Name", STRING, true);
			resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, OS_ARCH, "OS Arch", STRING, true);
			resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, OS_VERSION, "OS Version", STRING, true);
			resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, JAVA_VENDOR, "Java Vendor", STRING, true);
			resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, JAVA_VERSION, "Java Version", STRING,
					true);

			//
			resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, AVAILABLE_PROCESSORS,
					"Available Processors", INTEGER, true);

			//
			resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, SYSTEM_LOAD_AVERAGE, "System Load Average",
					FLOAT, true);
			if (this.systemLoadAverageState)
				setOrAddState(osJ, resource, SYSTEM_LOAD_AVERAGE, SYSTEM_LOAD_AVERAGE + "State", "System Load Average",
						FLOAT);

			//
			resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, START_TIME, "Start Time", DATE, true);

			//
			resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, UPTIME, "Uptime", LONG, true);
		}

		if (systemStateJ.has(MEMORY)) {
			JsonObject memoryJ = systemStateJ.get(MEMORY).getAsJsonObject();

			String bagId = MEMORY;
			String bagName = "Memory";
			String bagType = "Memory";

			//
			resource.setOrAddParamFromFlatJson(memoryJ, bagId, bagName, bagType, TOTAL_PHYSICAL_MEMORY_SIZE,
					"Total Physical Memory Size", LONG, true);

			//
			resource.setOrAddParamFromFlatJson(memoryJ, bagId, bagName, bagType, FREE_PHYSICAL_MEMORY_SIZE,
					"Free Physical Memory Size", LONG, true);
			if (this.freePhysicalMemorySizeState)
				setOrAddState(memoryJ, resource, FREE_PHYSICAL_MEMORY_SIZE, FREE_PHYSICAL_MEMORY_SIZE + "State",
						"Free Physical Memory Size", LONG);

			//
			resource.setOrAddParamFromFlatJson(memoryJ, bagId, bagName, bagType, FREE_SWAP_SPACE_SIZE,
					"Free Swap Space Size", LONG, true);
			if (this.freeSwapSpaceSizeState)
				setOrAddState(memoryJ, resource, FREE_SWAP_SPACE_SIZE, FREE_SWAP_SPACE_SIZE + "State",
						"Free Swap Space Size", LONG);

			//
			resource.setOrAddParamFromFlatJson(memoryJ, bagId, bagName, bagType, COMMITTED_VIRTUAL_MEMORY_SIZE,
					"Committed Virtual Memory Size", LONG, true);
			if (this.committedVirtualMemorySizeState)
				setOrAddState(memoryJ, resource, COMMITTED_VIRTUAL_MEMORY_SIZE, COMMITTED_VIRTUAL_MEMORY_SIZE + "State",
						"Committed Virtual Memory Size", LONG);

			//
			resource.setOrAddParamFromFlatJson(memoryJ, bagId, bagName, bagType, HEAP_MEMORY_USAGE_INIT,
					"Heap Memory Usage Init", LONG, true);
			if (this.heapMemoryUsageInitState)
				setOrAddState(memoryJ, resource, HEAP_MEMORY_USAGE_INIT, HEAP_MEMORY_USAGE_INIT + "State",
						"Heap Memory Usage Init", LONG);

			//
			resource.setOrAddParamFromFlatJson(memoryJ, bagId, bagName, bagType, HEAP_MEMORY_USAGE_USED,
					"Heap Memory Usage Used", LONG, true);
			if (this.heapMemoryUsageUsedState)
				setOrAddState(memoryJ, resource, HEAP_MEMORY_USAGE_USED, HEAP_MEMORY_USAGE_USED + "State",
						"Heap Memory Usage Used", LONG);

			//
			resource.setOrAddParamFromFlatJson(memoryJ, bagId, bagName, bagType, HEAP_MEMORY_USAGE_MAX,
					"Heap Memory Usage Max", LONG, true);
			if (this.heapMemoryUsageMaxState)
				setOrAddState(memoryJ, resource, HEAP_MEMORY_USAGE_MAX, HEAP_MEMORY_USAGE_MAX + "State",
						"Heap Memory Usage Max", LONG);

			//
			resource.setOrAddParamFromFlatJson(memoryJ, bagId, bagName, bagType, HEAP_MEMORY_USAGE_COMMITTED,
					"Heap Memory Usage Committed", LONG, true);
			if (this.heapMemoryUsageCommittedState)
				setOrAddState(memoryJ, resource, HEAP_MEMORY_USAGE_COMMITTED, HEAP_MEMORY_USAGE_COMMITTED + "State",
						"Heap Memory Usage Committed", LONG);
		}

		if (systemStateJ.has(ROOTS)) {
			JsonArray rootsJ = systemStateJ.get(ROOTS).getAsJsonArray();
			for (JsonElement rootE : rootsJ) {
				JsonObject rootJ = rootE.getAsJsonObject();

				String path = rootJ.get(PATH).getAsString();
				if (path.equals("/"))
					path = "root";

				String bagId = path;
				String bagName = "Root " + path;
				String bagType = "Root";

				resource.setOrAddParamFromFlatJson(rootJ, bagId, bagName, bagType, PATH, "Path", STRING, true);

				//
				resource.setOrAddParamFromFlatJson(rootJ, bagId, bagName, bagType, USABLE_SPACE, "Usable Space", LONG,
						true);
				if (this.usableSpaceState)
					setOrAddState(rootJ, resource, USABLE_SPACE, bagId + USABLE_SPACE + "State", "Usable Space", LONG);

				//
				resource.setOrAddParamFromFlatJson(rootJ, bagId, bagName, bagType, USED_SPACE, "Used Space", LONG,
						true);
				if (this.usedSpaceState)
					setOrAddState(rootJ, resource, USED_SPACE, bagId + USED_SPACE + "State", "Used Space", LONG);

				//
				resource.setOrAddParamFromFlatJson(rootJ, bagId, bagName, bagType, FREE_SPACE, "Free Space", LONG,
						true);
				if (this.freeSpaceState)
					setOrAddState(rootJ, resource, FREE_SPACE, bagId + FREE_SPACE + "State", "Free Space", LONG);

				//
				resource.setOrAddParamFromFlatJson(rootJ, bagId, bagName, bagType, TOTAL_SPACE, "Total Space", LONG,
						true);
			}
		}
	}

	private void setOrAddState(JsonObject jsonObject, Resource resource, String jsonId, String stateId,
			String stateName, StrolchValueType type) {

		StrolchTimedState<? extends IValue<?>> state = resource.getTimedState(stateId);
		if (state == null) {
			state = type.timedStateInstance();
			state.setId(stateId);
			state.setName(stateName);

			resource.addTimedState(state);
		}

		boolean valueNotSet = !jsonObject.has(jsonId) || jsonObject.get(jsonId).isJsonNull();
		if (valueNotSet)
			return;

		state.setStateFromStringAt(this.stateTime, jsonObject.get(jsonId).getAsString());

		if (this.compactStates)
			state.getTimeEvolution().compact();
	}
}
