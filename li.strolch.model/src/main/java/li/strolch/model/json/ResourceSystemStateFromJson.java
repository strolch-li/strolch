package li.strolch.model.json;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;
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
import li.strolch.model.visitor.SetStateValueVisitor;
import li.strolch.utils.DataUnit;

public class ResourceSystemStateFromJson {

	private long stateTime = System.currentTimeMillis();

	private DataUnit memoryRoundingUnit = DataUnit.Bytes;
	private DataUnit storageSpaceRoundingUnit = DataUnit.Bytes;

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

	public ResourceSystemStateFromJson withMemoryRounding(DataUnit dataUnit) {
		this.memoryRoundingUnit = dataUnit;
		return this;
	}

	public ResourceSystemStateFromJson withStorageSpaceRounding(DataUnit dataUnit) {
		this.storageSpaceRoundingUnit = dataUnit;
		return this;
	}

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

		if (systemStateJ.has(OPERATING_SYSTEM))
			handleOperatingSystem(systemStateJ, resource);

		if (systemStateJ.has(MEMORY))
			handleMemory(systemStateJ, resource);

		if (systemStateJ.has(ROOTS))
			handleRoots(systemStateJ, resource);
	}

	private void handleOperatingSystem(JsonObject systemStateJ, Resource resource) {
		JsonObject osJ = systemStateJ.get(OPERATING_SYSTEM).getAsJsonObject();

		String bagId = OPERATING_SYSTEM;
		String bagName = "Operating System";
		String bagType = "OperatingSystem";
		resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, OS_NAME, "OS Name", STRING, true);
		resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, OS_ARCH, "OS Arch", STRING, true);
		resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, OS_VERSION, "OS Version", STRING, true);
		resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, JAVA_VENDOR, "Java Vendor", STRING, true);
		resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, JAVA_VERSION, "Java Version", STRING, true);
		resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, START_TIME, "Start Time", DATE, true);
		resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, UPTIME, "Uptime", LONG, true);

		//
		resource.setOrAddParamFromFlatJson(osJ, bagId, bagName, bagType, AVAILABLE_PROCESSORS, "Available Processors",
				INTEGER, true);

		//
		if (osJ.has(SYSTEM_LOAD_AVERAGE) && !osJ.get(SYSTEM_LOAD_AVERAGE).isJsonNull()) {
			double value = osJ.get(SYSTEM_LOAD_AVERAGE).getAsDouble();
			resource.setOrAddParam(bagId, bagName, bagType, SYSTEM_LOAD_AVERAGE, "System Load Average", "SystemLoad",
					"SystemLoad", FLOAT, value, true);
			if (this.systemLoadAverageState)
				setOrAddState(resource, SYSTEM_LOAD_AVERAGE + "State", "System Load Average", INTERPRETATION_NONE,
						UOM_NONE, FLOAT, value);
		}
	}

	private void handleMemory(JsonObject systemStateJ, Resource resource) {
		JsonObject memoryJ = systemStateJ.get(MEMORY).getAsJsonObject();

		String bagId = MEMORY;
		String bagName = "Memory";
		String bagType = "Memory";

		//
		if (memoryJ.has(TOTAL_PHYSICAL_MEMORY_SIZE) && !memoryJ.get(TOTAL_PHYSICAL_MEMORY_SIZE).isJsonNull()) {
			long value = this.memoryRoundingUnit.roundBytesToUnit(memoryJ.get(TOTAL_PHYSICAL_MEMORY_SIZE).getAsLong());
			resource.setOrAddParam(bagId, bagName, bagType, TOTAL_PHYSICAL_MEMORY_SIZE, "Total Physical Memory Size",
					this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value, true);
		}

		//
		if (memoryJ.has(FREE_PHYSICAL_MEMORY_SIZE) && !memoryJ.get(FREE_PHYSICAL_MEMORY_SIZE).isJsonNull()) {
			long value = this.memoryRoundingUnit.roundBytesToUnit(memoryJ.get(FREE_PHYSICAL_MEMORY_SIZE).getAsLong());
			resource.setOrAddParam(bagId, bagName, bagType, FREE_PHYSICAL_MEMORY_SIZE, "Free Physical Memory Size",
					this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value, true);
			if (this.freePhysicalMemorySizeState)
				setOrAddState(resource, FREE_PHYSICAL_MEMORY_SIZE + "State", "Free Physical Memory Size",
						this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value);
		}

		//
		if (memoryJ.has(FREE_SWAP_SPACE_SIZE) && !memoryJ.get(FREE_SWAP_SPACE_SIZE).isJsonNull()) {
			long value = this.memoryRoundingUnit.roundBytesToUnit(memoryJ.get(FREE_SWAP_SPACE_SIZE).getAsLong());
			resource.setOrAddParam(bagId, bagName, bagType, FREE_SWAP_SPACE_SIZE, "Free Swap Space Size",
					this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value, true);
			if (this.freeSwapSpaceSizeState)
				setOrAddState(resource, FREE_SWAP_SPACE_SIZE + "State", "Free Swap Space Size",
						this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value);
		}

		//
		if (memoryJ.has(COMMITTED_VIRTUAL_MEMORY_SIZE) && !memoryJ.get(COMMITTED_VIRTUAL_MEMORY_SIZE).isJsonNull()) {
			long value = this.memoryRoundingUnit
					.roundBytesToUnit(memoryJ.get(COMMITTED_VIRTUAL_MEMORY_SIZE).getAsLong());
			resource.setOrAddParam(bagId, bagName, bagType, COMMITTED_VIRTUAL_MEMORY_SIZE,
					"Committed Virtual Memory Size", this.memoryRoundingUnit.getInterpretation(),
					this.memoryRoundingUnit.getUom(), LONG, value, true);
			if (this.committedVirtualMemorySizeState)
				setOrAddState(resource, COMMITTED_VIRTUAL_MEMORY_SIZE + "State", "Committed Virtual Memory Size",
						this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value);
		}

		//
		if (memoryJ.has(HEAP_MEMORY_USAGE_INIT) && !memoryJ.get(HEAP_MEMORY_USAGE_INIT).isJsonNull()) {
			long value = this.memoryRoundingUnit.roundBytesToUnit(memoryJ.get(HEAP_MEMORY_USAGE_INIT).getAsLong());
			resource.setOrAddParam(bagId, bagName, bagType, HEAP_MEMORY_USAGE_INIT, "Heap Memory Usage Init",
					this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value, true);
			if (this.heapMemoryUsageInitState)
				setOrAddState(resource, HEAP_MEMORY_USAGE_INIT + "State", "Heap Memory Usage Init",
						this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value);
		}

		//
		if (memoryJ.has(HEAP_MEMORY_USAGE_USED) && !memoryJ.get(HEAP_MEMORY_USAGE_USED).isJsonNull()) {
			long value = this.memoryRoundingUnit.roundBytesToUnit(memoryJ.get(HEAP_MEMORY_USAGE_USED).getAsLong());
			resource.setOrAddParam(bagId, bagName, bagType, HEAP_MEMORY_USAGE_USED, "Heap Memory Usage Used",
					this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value, true);
			if (this.heapMemoryUsageUsedState)
				setOrAddState(resource, HEAP_MEMORY_USAGE_USED + "State", "Heap Memory Usage Used",
						this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value);
		}

		//
		if (memoryJ.has(HEAP_MEMORY_USAGE_MAX) && !memoryJ.get(HEAP_MEMORY_USAGE_MAX).isJsonNull()) {
			long value = this.memoryRoundingUnit.roundBytesToUnit(memoryJ.get(HEAP_MEMORY_USAGE_MAX).getAsLong());
			resource.setOrAddParam(bagId, bagName, bagType, HEAP_MEMORY_USAGE_MAX, "Heap Memory Usage Max",
					this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value, true);
			if (this.heapMemoryUsageMaxState)
				setOrAddState(resource, HEAP_MEMORY_USAGE_MAX + "State", "Heap Memory Usage Max",
						this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value);
		}

		//
		if (memoryJ.has(HEAP_MEMORY_USAGE_COMMITTED) && !memoryJ.get(HEAP_MEMORY_USAGE_COMMITTED).isJsonNull()) {
			long value = this.memoryRoundingUnit.roundBytesToUnit(memoryJ.get(HEAP_MEMORY_USAGE_COMMITTED).getAsLong());
			resource.setOrAddParam(bagId, bagName, bagType, HEAP_MEMORY_USAGE_COMMITTED, "Heap Memory Usage Committed",
					this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value, true);
			if (this.heapMemoryUsageCommittedState)
				setOrAddState(resource, HEAP_MEMORY_USAGE_COMMITTED + "State", "Heap Memory Usage Committed",
						this.memoryRoundingUnit.getInterpretation(), this.memoryRoundingUnit.getUom(), LONG, value);
		}
	}

	private void handleRoots(JsonObject systemStateJ, Resource resource) {
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
			if (rootJ.has(TOTAL_SPACE) && !rootJ.get(TOTAL_SPACE).isJsonNull()) {
				long value = this.storageSpaceRoundingUnit.roundBytesToUnit(rootJ.get(TOTAL_SPACE).getAsLong());
				resource.setOrAddParam(bagId, bagName, bagType, TOTAL_SPACE, "Total Space",
						this.storageSpaceRoundingUnit.getInterpretation(), this.storageSpaceRoundingUnit.getUom(), LONG,
						value, true);
			}

			//
			if (rootJ.has(USABLE_SPACE) && !rootJ.get(USABLE_SPACE).isJsonNull()) {
				long value = this.storageSpaceRoundingUnit.roundBytesToUnit(rootJ.get(USABLE_SPACE).getAsLong());
				resource.setOrAddParam(bagId, bagName, bagType, USABLE_SPACE, "Usable Space",
						this.storageSpaceRoundingUnit.getInterpretation(), this.storageSpaceRoundingUnit.getUom(), LONG,
						value, true);
				if (this.usableSpaceState)
					setOrAddState(resource, bagId + USABLE_SPACE + "State", "Usable Space",
							this.storageSpaceRoundingUnit.getInterpretation(), this.storageSpaceRoundingUnit.getUom(),
							LONG, value);
			}

			//
			if (rootJ.has(USED_SPACE) && !rootJ.get(USED_SPACE).isJsonNull()) {
				long value = this.storageSpaceRoundingUnit.roundBytesToUnit(rootJ.get(USED_SPACE).getAsLong());
				resource.setOrAddParam(bagId, bagName, bagType, USED_SPACE, "Used Space",
						this.storageSpaceRoundingUnit.getInterpretation(), this.storageSpaceRoundingUnit.getUom(), LONG,
						value, true);
				if (this.usedSpaceState)
					setOrAddState(resource, bagId + USED_SPACE + "State", "Used Space",
							this.storageSpaceRoundingUnit.getInterpretation(), this.storageSpaceRoundingUnit.getUom(),
							LONG, value);
			}

			//
			if (rootJ.has(FREE_SPACE) && !rootJ.get(FREE_SPACE).isJsonNull()) {
				long value = this.storageSpaceRoundingUnit.roundBytesToUnit(rootJ.get(FREE_SPACE).getAsLong());
				resource.setOrAddParam(bagId, bagName, bagType, FREE_SPACE, "Free Space",
						this.storageSpaceRoundingUnit.getInterpretation(), this.storageSpaceRoundingUnit.getUom(), LONG,
						value, true);
				if (this.freeSpaceState)
					setOrAddState(resource, bagId + FREE_SPACE + "State", "Free Space",
							this.storageSpaceRoundingUnit.getInterpretation(), this.storageSpaceRoundingUnit.getUom(),
							LONG, value);
			}
		}
	}

	private void setOrAddState(Resource resource, String stateId, String stateName, String interpretation, String uom,
			StrolchValueType type, Object value) {

		if (value == null)
			return;

		StrolchTimedState<? extends IValue<?>> state = resource.getTimedState(stateId);
		if (state == null) {
			state = type.timedStateInstance();
			state.setId(stateId);
			state.setName(stateName);
			state.setInterpretation(interpretation);
			state.setUom(uom);

			resource.addTimedState(state);
		}

		state.accept(new SetStateValueVisitor(this.stateTime, value));

		if (this.compactStates)
			state.getTimeEvolution().compact();
	}
}
