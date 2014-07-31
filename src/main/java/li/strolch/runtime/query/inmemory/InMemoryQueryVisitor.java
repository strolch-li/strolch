/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.runtime.query.inmemory;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.query.AndSelection;
import li.strolch.model.query.BooleanSelection;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.NotSelection;
import li.strolch.model.query.OrSelection;
import li.strolch.model.query.ParameterSelection.BooleanParameterSelection;
import li.strolch.model.query.ParameterSelection.DateParameterSelection;
import li.strolch.model.query.ParameterSelection.DateRangeParameterSelection;
import li.strolch.model.query.ParameterSelection.FloatParameterSelection;
import li.strolch.model.query.ParameterSelection.IntegerParameterSelection;
import li.strolch.model.query.ParameterSelection.LongParameterSelection;
import li.strolch.model.query.ParameterSelection.StringListParameterSelection;
import li.strolch.model.query.ParameterSelection.StringParameterSelection;
import li.strolch.model.query.ParameterSelectionVisitor;
import li.strolch.model.query.Selection;
import li.strolch.model.query.StrolchElementSelectionVisitor;
import li.strolch.persistence.api.StrolchDao;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class InMemoryQueryVisitor<T extends GroupedParameterizedElement, S extends StrolchDao<?>> implements
		StrolchElementSelectionVisitor, ParameterSelectionVisitor {

	protected Navigator<T> navigator;
	protected List<Selector<T>> selectors;

	public InMemoryQueryVisitor() {
		this.selectors = new ArrayList<>();
	}

	public List<Selector<T>> getSelectors() {
		return this.selectors;
	}

	public Navigator<T> getNavigator() {
		return this.navigator;
	}

	/**
	 * Returns a new instance of this concrete type for performing recursive {@link BooleanSelection BooleanSelections}
	 * 
	 * @return a new instance of this concrete type
	 */
	protected abstract InMemoryQueryVisitor<T, S> newInstance();

	@Override
	public void visitAnd(AndSelection andSelection) {
		InMemoryQueryVisitor<T, S> query = newInstance();
		List<Selection> selections = andSelection.getSelections();
		for (Selection selection : selections) {
			selection.accept(query);
		}
		AndSelector<T> andSelector = new AndSelector<>(query.getSelectors());
		this.selectors.add(andSelector);
	}

	@Override
	public void visitOr(OrSelection orSelection) {
		InMemoryQueryVisitor<T, S> query = newInstance();
		List<Selection> selections = orSelection.getSelections();
		for (Selection selection : selections) {
			selection.accept(query);
		}
		OrSelector<T> orSelector = new OrSelector<>(query.getSelectors());
		this.selectors.add(orSelector);
	}

	@Override
	public void visitNot(NotSelection notSelection) {
		InMemoryQueryVisitor<T, S> query = newInstance();
		List<Selection> selections = notSelection.getSelections();
		for (Selection selection : selections) {
			selection.accept(query);
		}
		List<Selector<T>> notSelectors = query.getSelectors();
		if (!notSelectors.isEmpty()) {
			NotSelector<T> notSelector = new NotSelector<>(notSelectors.get(0));
			this.selectors.add(notSelector);
		}
	}

	@Override
	public void visit(IdSelection selection) {
		this.selectors.add(new IdSelector<T>(selection.getIds()));
	}

	@Override
	public void visit(NameSelection selection) {
		this.selectors.add(new NameSelector<T>(selection.getName()));
	}

	@Override
	public void visit(StringParameterSelection selection) {
		this.selectors.add(ParameterSelector
				.<T> stringSelector(selection.getBagKey(), selection.getParamKey(), selection.getValue())
				.contains(selection.isContains()).caseInsensitive(selection.isCaseInsensitive()));
	}

	@Override
	public void visit(IntegerParameterSelection selection) {
		this.selectors.add(ParameterSelector.<T> integerSelector(selection.getBagKey(), selection.getParamKey(),
				selection.getValue()));
	}

	@Override
	public void visit(BooleanParameterSelection selection) {
		this.selectors.add(ParameterSelector.<T> booleanSelector(selection.getBagKey(), selection.getParamKey(),
				selection.getValue()));
	}

	@Override
	public void visit(LongParameterSelection selection) {
		this.selectors.add(ParameterSelector.<T> longSelector(selection.getBagKey(), selection.getParamKey(),
				selection.getValue()));
	}

	@Override
	public void visit(FloatParameterSelection selection) {
		this.selectors.add(ParameterSelector.<T> floatSelector(selection.getBagKey(), selection.getParamKey(),
				selection.getValue()));
	}

	@Override
	public void visit(DateParameterSelection selection) {
		this.selectors.add(ParameterSelector.<T> dateSelector(selection.getBagKey(), selection.getParamKey(),
				selection.getValue()));
	}
	
	@Override
	public void visit(DateRangeParameterSelection selection) {
		this.selectors.add(ParameterSelector.<T> dateRangeSelector(selection.getBagKey(), selection.getParamKey(),
				selection.getFrom(), selection.getTo()));
	}

	@Override
	public void visit(StringListParameterSelection selection) {
		this.selectors.add(ParameterSelector.<T> stringListSelector(selection.getBagKey(), selection.getParamKey(),
				selection.getValue()));
	}
}
