/*
 * Copyright (c) 2026 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.model.parameter;

import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import org.junit.Test;
import org.openjdk.jol.info.GraphLayout;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import static org.junit.Assume.assumeTrue;

/**
 * Opt-in baseline for the current generic Parameter model memory footprint.
 */
public class ParameterMemoryFootprintBaselineTest {

	private static final String ENABLED_PROPERTY = "parameterMemoryFootprintBaseline.enabled";
	private static final String RESOURCE_COUNT_PROPERTY = "parameterMemoryFootprintBaseline.resources";
	private static final String OPERATIONS_PROPERTY = "parameterMemoryFootprintBaseline.operations";

	private static final int DEFAULT_RESOURCE_COUNT = 100_000;
	private static final int DEFAULT_OPERATIONS = 50_000;
	private static final int PARAMETER_COUNT = 20;
	private static final ZonedDateTime PICKING_DATE = ZonedDateTime.of(2026, 7, 11, 12, 0, 0, 0, ZoneOffset.UTC);

	private static volatile Object sink;

	@Test
	public void runBaseline() {
		assumeTrue("Enable with -D" + ENABLED_PROPERTY + "=true", Boolean.getBoolean(ENABLED_PROPERTY));

		int resourceCount = Integer.getInteger(RESOURCE_COUNT_PROPERTY, DEFAULT_RESOURCE_COUNT);
		int operations = Integer.getInteger(OPERATIONS_PROPERTY, DEFAULT_OPERATIONS);
		List<Resource> resources = createPickingItems(resourceCount);

		GraphLayout layout = GraphLayout.parseInstance(resources);
		long totalBytes = layout.totalSize();
		long totalParameters = (long) resourceCount * PARAMETER_COUNT;

		System.out.println("\nParameter memory footprint baseline");
		System.out.println("===================================");
		System.out.println("resources              : " + resourceCount);
		System.out.println("parameters             : " + totalParameters);
		System.out.println("retained heap bytes    : " + totalBytes);
		System.out.println("retained heap MiB      : " + formatMib(totalBytes));
		System.out.println("bytes per resource     : " + formatDouble(totalBytes / (double) resourceCount));
		System.out.println("bytes per parameter    : " + formatDouble(totalBytes / (double) totalParameters));
		System.out.println("\nObject counts and retained heap by class");
		System.out.println(layout.toFootprint());

		System.out.println("\nLookup and mutation micro-scenarios");
		System.out.println("getParameter ns/op     : " + formatDouble(timeGetParameter(resources, operations) / (double) operations));
		System.out.println("getString ns/op        : " + formatDouble(timeGetString(resources, operations) / (double) operations));
		System.out.println("setValue ns/op         : " + formatDouble(timeSetValue(resources, operations) / (double) operations));
		System.out.println("clear ns/op            : " + formatDouble(timeClear(resources, operations) / (double) operations));
		int cloneOperations = Math.min(operations, resourceCount);
		System.out.println("getClone ns/op         : " + formatDouble(timeGetClone(resources, cloneOperations) /
				(double) cloneOperations));
	}

	private static List<Resource> createPickingItems(int resourceCount) {
		List<Resource> resources = new ArrayList<>(resourceCount);
		for (int i = 0; i < resourceCount; i++) {
			Resource resource = new Resource("PickingItem-" + i, "Picking Item " + i, "PickingItem");
			ParameterBag bag = new ParameterBag("parameters", "Parameters", "PickingItemParameters");
			bag.addParameter(new StringParameter("sku", "SKU", "SKU-0001"));
			bag.addParameter(new StringParameter("lot", "Lot", "LOT-A"));
			bag.addParameter(new StringParameter("location", "Location", "LOC-01"));
			bag.addParameter(new StringParameter("owner", "Owner", "atexxi"));
			bag.addParameter(new StringParameter("status", "Status", "OPEN"));
			bag.addParameter(new StringParameter("priority", "Priority", "NORMAL"));
			bag.addParameter(new IntegerParameter("quantity", "Quantity", 12));
			bag.addParameter(new IntegerParameter("pickedQuantity", "Picked Quantity", 0));
			bag.addParameter(new IntegerParameter("sequence", "Sequence", i));
			bag.addParameter(new LongParameter("weight", "Weight", 1_500L));
			bag.addParameter(new LongParameter("volume", "Volume", 2_500L));
			bag.addParameter(new FloatParameter("temperature", "Temperature", 18.5D));
			bag.addParameter(new FloatParameter("price", "Price", 42.25D));
			bag.addParameter(new BooleanParameter("fragile", "Fragile", false));
			bag.addParameter(new BooleanParameter("hazardous", "Hazardous", false));
			bag.addParameter(new DateParameter("created", "Created", PICKING_DATE));
			bag.addParameter(new DateParameter("due", "Due", PICKING_DATE.plusDays(1)));
			bag.addParameter(new StringListParameter("tags", "Tags", List.of("standard", "homogeneous")));
			bag.addParameter(new IntegerListParameter("allowedBins", "Allowed Bins", List.of(1, 2, 3)));
			bag.addParameter(new TextParameter("notes", "Notes", "Mostly identical synthetic PickingItem values"));
			resource.addParameterBag(bag);
			resources.add(resource);
		}
		return resources;
	}

	private static long timeGetParameter(List<Resource> resources, int operations) {
		long started = System.nanoTime();
		for (int i = 0; i < operations; i++)
			sink = resources.get(i % resources.size()).getParameterBag("parameters").getParameter("sku");
		return System.nanoTime() - started;
	}

	private static long timeGetString(List<Resource> resources, int operations) {
		long started = System.nanoTime();
		for (int i = 0; i < operations; i++)
			sink = resources.get(i % resources.size()).getParameterBag("parameters").getString("sku");
		return System.nanoTime() - started;
	}

	private static long timeSetValue(List<Resource> resources, int operations) {
		long started = System.nanoTime();
		for (int i = 0; i < operations; i++)
			resources.get(i % resources.size()).getParameterBag("parameters").setString("status", "OPEN-" + (i % 10));
		return System.nanoTime() - started;
	}

	private static long timeClear(List<Resource> resources, int operations) {
		long started = System.nanoTime();
		for (int i = 0; i < operations; i++) {
			StringParameter notes = resources.get(i % resources.size()).getParameterBag("parameters").getStringP("notes");
			notes.clear();
			notes.setValue("Mostly identical synthetic PickingItem values");
		}
		return System.nanoTime() - started;
	}

	private static long timeGetClone(List<Resource> resources, int operations) {
		long started = System.nanoTime();
		for (int i = 0; i < operations; i++)
			sink = resources.get(i % resources.size()).getClone();
		return System.nanoTime() - started;
	}

	private static String formatMib(long bytes) {
		return formatDouble(bytes / 1024.0D / 1024.0D);
	}

	private static String formatDouble(double value) {
		return String.format(Locale.ROOT, "%.2f", value);
	}

}