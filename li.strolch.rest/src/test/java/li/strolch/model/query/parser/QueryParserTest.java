package li.strolch.model.query.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import li.strolch.model.Resource;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.ParameterSelection.AnyTypeParameterSelection;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.Selection;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.utils.collections.MapOfSets;

public class QueryParserTest {

	@Test
	public void shouldIgnoreGibberish() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("sdf dfg3 !sdf", true, false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmpty() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("", true, false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmptyId() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("id:", true, false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmptyName() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("name:", true, false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmptyType() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("type:", true, false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmptyIdNameType() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("id: name: type:", true, false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseId() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("id:asd", true, false);
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(1, selections.size());
		assertEquals(IdSelection.class, selections.get(0).getClass());
		IdSelection idSelection = (IdSelection) selections.get(0);
		List<String> ids = idSelection.getIds();
		assertEquals(1, ids.size());
		assertEquals("asd", ids.get(0));
		assertFalse(query.hasNavigation());
	}

	@Test
	public void shouldParseIds() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("id:asd id:bbb", true, false);
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(1, selections.size());
		assertEquals(IdSelection.class, selections.get(0).getClass());
		IdSelection idSelection = (IdSelection) selections.get(0);
		List<String> ids = idSelection.getIds();
		assertEquals(2, ids.size());
		assertEquals("asd", ids.get(0));
		assertEquals("bbb", ids.get(1));
		assertFalse(query.hasNavigation());
	}

	@Test
	public void shouldParseName() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("name:asd", true, false);
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(1, selections.size());
		assertEquals(NameSelection.class, selections.get(0).getClass());
		assertEquals("asd", ((NameSelection) selections.get(0)).getName());
		assertFalse(query.hasNavigation());
	}

	@Test
	public void shouldParseNames() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("name:asd name:bbb", true, false);
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(2, selections.size());
		assertEquals(NameSelection.class, selections.get(0).getClass());
		assertEquals("asd", ((NameSelection) selections.get(0)).getName());

		assertEquals(NameSelection.class, selections.get(1).getClass());
		assertEquals("bbb", ((NameSelection) selections.get(1)).getName());
		assertFalse(query.hasNavigation());
	}

	@Test
	public void shouldParseType() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("type:asd", true, false);
		assertFalse(query.hasSelection());
		assertTrue(query.hasNavigation());
		StrolchTypeNavigation navigation = (StrolchTypeNavigation) query.getNavigation();
		assertEquals("asd", navigation.getType());
	}

	@Test
	public void shouldReplaceMultipleType() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("type:asd type:fff", true, false);
		assertFalse(query.hasSelection());
		assertTrue(query.hasNavigation());
		StrolchTypeNavigation navigation = (StrolchTypeNavigation) query.getNavigation();
		assertEquals("fff", navigation.getType());
	}

	@Test
	public void shouldParseIdNameType() {
		ResourceQuery<Resource> query = QueryParser
				.parseToResourceQuery("id:foo name:bar type:asd date:1970-01-01T01:00:00.000+01:00", true, false);
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(2, selections.size());
		assertEquals(IdSelection.class, selections.get(0).getClass());
		assertEquals(NameSelection.class, selections.get(1).getClass());
		assertTrue(query.hasNavigation());
	}

	@Test
	public void shouldParseWithWhitespace() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("  id:foo  name:bar  type:asd \t ", true,
				false);
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(2, selections.size());
		assertEquals(IdSelection.class, selections.get(0).getClass());
		assertEquals(NameSelection.class, selections.get(1).getClass());
		assertTrue(query.hasNavigation());
	}

	@Test
	public void shouldParserWithParameters() {
		MapOfSets<String, String> maps = new MapOfSets<>();
		maps.addElement("parameters", "email");
		maps.addElement("parameters", "date");

		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery(maps, true,
				"type:asd id:asd email:bla@dsfdfg.ch date:1970-01-01T01:00:00.000+01:00", false);
		assertTrue(query.hasNavigation());
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(3, selections.size());
		assertEquals(IdSelection.class, selections.get(0).getClass());
		assertEquals(AnyTypeParameterSelection.class, selections.get(1).getClass());
		assertEquals(AnyTypeParameterSelection.class, selections.get(2).getClass());

		AnyTypeParameterSelection sel = (AnyTypeParameterSelection) selections.get(1);
		assertEquals("parameters", sel.getBagKey());
		assertEquals("email", sel.getParamKey());
		assertEquals("bla@dsfdfg.ch", sel.getValue());

		sel = (AnyTypeParameterSelection) selections.get(2);
		assertEquals("parameters", sel.getBagKey());
		assertEquals("date", sel.getParamKey());
		assertEquals("1970-01-01T01:00:00.000+01:00", sel.getValue());
	}

	@Test
	public void shouldParserWithParametersAndNoPrefix() {
		MapOfSets<String, String> maps = new MapOfSets<>();
		maps.addElement("parameters", "email");
		maps.addElement("parameters", "date");

		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery(maps, false,
				"asd bla@dsfdfg.ch 1970-01-01T01:00:00.000+01:00", false);
		assertFalse(query.hasNavigation());
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(10, selections.size());

		AnyTypeParameterSelection sel;

		IdSelection idSel = (IdSelection) selections.get(0);
		assertEquals(Arrays.asList("asd", "bla@dsfdfg.ch", "1970-01-01T01:00:00.000+01:00"), idSel.getIds());

		NameSelection nameSel = (NameSelection) selections.get(1);
		assertEquals("asd", nameSel.getName());

		sel = (AnyTypeParameterSelection) selections.get(2);
		assertEquals("parameters", sel.getBagKey());
		assertEquals("date", sel.getParamKey());
		assertEquals("asd", sel.getValue());

		sel = (AnyTypeParameterSelection) selections.get(3);
		assertEquals("parameters", sel.getBagKey());
		assertEquals("email", sel.getParamKey());
		assertEquals("asd", sel.getValue());

		sel = (AnyTypeParameterSelection) selections.get(5);
		assertEquals("parameters", sel.getBagKey());
		assertEquals("date", sel.getParamKey());
		assertEquals("bla@dsfdfg.ch", sel.getValue());

		sel = (AnyTypeParameterSelection) selections.get(6);
		assertEquals("parameters", sel.getBagKey());
		assertEquals("email", sel.getParamKey());
		assertEquals("bla@dsfdfg.ch", sel.getValue());

		sel = (AnyTypeParameterSelection) selections.get(8);
		assertEquals("parameters", sel.getBagKey());
		assertEquals("date", sel.getParamKey());
		assertEquals("1970-01-01T01:00:00.000+01:00", sel.getValue());

		sel = (AnyTypeParameterSelection) selections.get(9);
		assertEquals("parameters", sel.getBagKey());
		assertEquals("email", sel.getParamKey());
		assertEquals("1970-01-01T01:00:00.000+01:00", sel.getValue());
	}
}
