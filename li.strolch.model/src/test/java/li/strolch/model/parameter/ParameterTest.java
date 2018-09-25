package li.strolch.model.parameter;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static li.strolch.model.ModelGenerator.*;
import static org.junit.Assert.*;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Date;

import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.junit.Before;
import org.junit.Test;

public class ParameterTest {

	private Resource resource;

	@Before
	public void before() {
		this.resource = ModelGenerator.createResource("test", "Test", "Test");
	}

	@Test
	public void testStringParam() {

		StringParameter other = new StringParameter("other", "other", "ab");
		StringParameter p = resource.getParameter(BAG_ID, PARAM_STRING_ID, true);

		assertEquals("Strolch", p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals("", p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));
	}

	@Test
	public void testIntegerParam() {

		IntegerParameter other = new IntegerParameter("other", "other", 42);
		IntegerParameter p = resource.getParameter(BAG_ID, PARAM_INTEGER_ID, true);

		assertEquals(Integer.valueOf(77), p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(Integer.valueOf(0), p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));

		p.add(2);
		assertEquals(Integer.valueOf(44), p.getValue());
		p.subtract(1);
		assertEquals(Integer.valueOf(43), p.getValue());
		p.multiply(2);
		assertEquals(Integer.valueOf(86), p.getValue());
		p.divide(2);
		assertEquals(Integer.valueOf(43), p.getValue());
		p.increment();
		assertEquals(Integer.valueOf(44), p.getValue());
		p.decrement();
		assertEquals(Integer.valueOf(43), p.getValue());
	}

	@Test
	public void testLongParam() {

		LongParameter other = new LongParameter("other", "other", 4242424242424242L);
		LongParameter p = resource.getParameter(BAG_ID, PARAM_LONG_ID, true);

		assertEquals(Long.valueOf(4453234566L), p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(Long.valueOf(0L), p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));

		p.add(2);
		assertEquals(Long.valueOf(4242424242424244L), p.getValue());
		p.subtract(1);
		assertEquals(Long.valueOf(4242424242424243L), p.getValue());
		p.multiply(2);
		assertEquals(Long.valueOf(8484848484848486L), p.getValue());
		p.divide(2);
		assertEquals(Long.valueOf(4242424242424243L), p.getValue());
		p.increment();
		assertEquals(Long.valueOf(4242424242424244L), p.getValue());
		p.decrement();
		assertEquals(Long.valueOf(4242424242424243L), p.getValue());
	}

	@Test
	public void testBooleanParam() {

		BooleanParameter other = new BooleanParameter("other", "other", true);
		BooleanParameter p = resource.getParameter(BAG_ID, PARAM_BOOLEAN_ID, true);

		assertEquals(Boolean.TRUE, p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(Boolean.FALSE, p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));

		p.flip();
		assertEquals(Boolean.FALSE, p.getValue());
	}

	@Test
	public void testFloatParam() {

		FloatParameter other = new FloatParameter("other", "other", 42.42D);
		FloatParameter p = resource.getParameter(BAG_ID, PARAM_FLOAT_ID, true);

		assertEquals(Double.valueOf(44.3), p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(Double.valueOf(0.0D), p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));

		p.add(2.1D);
		assertEquals(Double.valueOf(44.52D), p.getValue());
		p.subtract(1.2D);
		assertEquals(Double.valueOf(43.32D), p.getValue());
		p.multiply(2.2D);
		assertEquals(Double.valueOf(95.304D), p.getValue());
		p.divide(2.4D);
		assertEquals(Double.valueOf(39.71D), p.getValue());
		p.increment();
		assertEquals(Double.valueOf(40.71D), p.getValue());
		p.decrement();
		assertEquals(Double.valueOf(39.71D), p.getValue());
	}

	@Test
	public void testDateParam() {

		DateParameter other = new DateParameter("other", "other", new Date(42L));
		DateParameter p = resource.getParameter(BAG_ID, PARAM_DATE_ID, true);

		assertEquals(new Date(1354295525628L), p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(new Date(0L), p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));

		LocalDateTime now = LocalDateTime.now();
		p.setValueFromLocalDateTime(now);
		LocalDateTime localDateTime = p.toLocalDateTime();
		assertEquals(now, localDateTime);

		assertTrue(p.isEqualTo(now));

		ZonedDateTime now1 = ZonedDateTime.now();
		p.setValueFromZonedDateTime(now1);
		ZonedDateTime zonedDateTime = p.toZonedDateTime();
		assertEquals(now1, zonedDateTime);
		assertTrue(p.isEqualTo(now1));
	}

	@Test
	public void testDurationParam() {

		DurationParameter other = new DurationParameter("other", "other", 42L);
		DurationParameter p = resource.getParameter(BAG_ID, PARAM_DURATION_ID, true);

		assertEquals(Long.valueOf(ISO8601FormatFactory.getInstance().getDurationFormat().parse("P1D")), p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(Long.valueOf(0L), p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));
	}

	@Test
	public void testStringListParam() {

		StringListParameter other = new StringListParameter("other", "other", asList("ab", "ba"));
		StringListParameter p = resource.getParameter(BAG_ID, PARAM_LIST_STRING_ID, true);

		assertEquals(asList("Hello", "World"), p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(emptyList(), p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));

		p.addValue("ca");
		assertEquals(asList("ab", "ba", "ca"), p.getValue());
		p.removeValue("ba");
		assertEquals(asList("ab", "ca"), p.getValue());
		assertTrue(p.contains("ab"));
		assertFalse(p.contains("ba"));

		p.addValueIfNotContains("ba");
		p.addValueIfNotContains("ba");
		assertTrue(p.containsAll(asList("ab", "ba", "ca")));
		assertEquals(asList("ab", "ca", "ba"), p.getValue());
	}

	@Test
	public void testIntegerListParam() {

		IntegerListParameter other = new IntegerListParameter("other", "other", asList(42, 43));
		IntegerListParameter p = resource.getParameter(BAG_ID, PARAM_LIST_INTEGER_ID, true);

		assertEquals(asList(5, 10, 15), p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(emptyList(), p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));

		p.addValue(44);
		assertEquals(asList(42, 43, 44), p.getValue());
		p.removeValue(43);
		assertEquals(asList(42, 44), p.getValue());
		assertTrue(p.contains(44));
		assertFalse(p.contains(43));

		p.addValueIfNotContains(43);
		p.addValueIfNotContains(43);
		assertTrue(p.containsAll(asList(42, 43, 44)));
		assertEquals(asList(42, 44, 43), p.getValue());
	}

	@Test
	public void testLongListParam() {

		LongListParameter other = new LongListParameter("other", "other", asList(4242424242424242L, 4343434343434343L));
		LongListParameter p = resource.getParameter(BAG_ID, PARAM_LIST_LONG_ID, true);

		assertEquals(asList(7L, 12L, 17L), p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(emptyList(), p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));

		p.addValue(5656565656565656L);
		assertEquals(asList(4242424242424242L, 4343434343434343L, 5656565656565656L), p.getValue());
		p.removeValue(4343434343434343L);
		assertEquals(asList(4242424242424242L, 5656565656565656L), p.getValue());
		assertTrue(p.contains(5656565656565656L));
		assertFalse(p.contains(4343434343434343L));

		p.addValueIfNotContains(4343434343434343L);
		p.addValueIfNotContains(4343434343434343L);
		assertTrue(p.containsAll(asList(4242424242424242L, 5656565656565656L, 4343434343434343L)));
		assertEquals(asList(4242424242424242L, 5656565656565656L, 4343434343434343L), p.getValue());
	}

	@Test
	public void testFloatListParam() {

		FloatListParameter other = new FloatListParameter("other", "other", asList(42.42D, 43.43D));
		FloatListParameter p = resource.getParameter(BAG_ID, PARAM_LIST_FLOAT_ID, true);

		assertEquals(asList(6.0, 11.0, 16.0), p.getValue());

		p.clear();
		assertTrue(p.isEmpty());
		assertEquals(emptyList(), p.getValue());

		p.setValue(other);
		assertTrue(p.isEqualTo(other.getValue()));
		assertTrue(p.isEqualTo(other));

		p.addValue(88.88D);
		assertEquals(asList(42.42D, 43.43D, 88.88D), p.getValue());
		p.removeValue(43.43D);
		assertEquals(asList(42.42D, 88.88D), p.getValue());
		assertTrue(p.contains(88.88D));
		assertFalse(p.contains(43.43D));

		p.addValueIfNotContains(43.43D);
		p.addValueIfNotContains(43.43D);
		assertTrue(p.containsAll(asList(42.42D, 88.88D, 43.43D)));
		assertEquals(asList(42.42D, 88.88D, 43.43D), p.getValue());
	}
}
