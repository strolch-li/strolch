package li.strolch.model.query.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

import li.strolch.model.Resource;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.Selection;
import li.strolch.model.query.StrolchTypeNavigation;

public class QueryParserTest {

	@Test
	public void shouldIgnoreGibberish() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("sdf dfg3 !sdf", false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmpty() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("", false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmptyId() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("id:", false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmptyName() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("name:", false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmptyType() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("type:", false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseEmptyIdNameType() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("id: name: type:", false);
		assertFalse(query.hasNavigation());
		assertFalse(query.hasSelection());
	}

	@Test
	public void shouldParseId() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("id:asd", false);
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
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("id:asd id:bbb", false);
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
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("name:asd", false);
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(1, selections.size());
		assertEquals(NameSelection.class, selections.get(0).getClass());
		assertEquals("asd", ((NameSelection) selections.get(0)).getName());
		assertFalse(query.hasNavigation());
	}

	@Test
	public void shouldParseNames() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("name:asd name:bbb", false);
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
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("type:asd", false);
		assertFalse(query.hasSelection());
		assertTrue(query.hasNavigation());
		StrolchTypeNavigation navigation = (StrolchTypeNavigation) query.getNavigation();
		assertEquals("asd", navigation.getType());
	}

	@Test
	public void shouldReplaceMultipleType() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("type:asd type:fff", false);
		assertFalse(query.hasSelection());
		assertTrue(query.hasNavigation());
		StrolchTypeNavigation navigation = (StrolchTypeNavigation) query.getNavigation();
		assertEquals("fff", navigation.getType());
	}

	@Test
	public void shouldParseIdNameType() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("id:foo name:bar type:asd", false);
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(2, selections.size());
		assertEquals(IdSelection.class, selections.get(0).getClass());
		assertEquals(NameSelection.class, selections.get(1).getClass());
		assertTrue(query.hasNavigation());
	}

	@Test
	public void shouldParseWithWhitespace() {
		ResourceQuery<Resource> query = QueryParser.parseToResourceQuery("  id:foo  name:bar  type:asd \t ", false);
		OrSelection or = (OrSelection) query.getSelection();
		List<Selection> selections = or.getSelections();
		assertEquals(2, selections.size());
		assertEquals(IdSelection.class, selections.get(0).getClass());
		assertEquals(NameSelection.class, selections.get(1).getClass());
		assertTrue(query.hasNavigation());
	}
}
