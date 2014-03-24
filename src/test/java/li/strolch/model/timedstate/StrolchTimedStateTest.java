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
package li.strolch.model.timedstate;

import java.util.HashSet;
import java.util.Set;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.impl.AString;
import li.strolch.model.timevalue.impl.BooleanValue;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.StringSetValue;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchTimedStateTest {

    @Test
    public void testFloatState() {

        Resource myRes = ModelGenerator.createResource("@1", "Test With States", "Stated");

        FloatTimedState floatState = myRes.getTimedState(ModelGenerator.STATE_FLOAT_ID);
        ITimeValue<FloatValue> valueAt0 = floatState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_0);
        assertEquals(ModelGenerator.STATE_FLOAT_TIME_0, valueAt0.getValue().getValue());

        ITimeValue<FloatValue> valueAt10 = floatState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_10);
        assertEquals(ModelGenerator.STATE_FLOAT_TIME_10, valueAt10.getValue().getValue());

        ITimeValue<FloatValue> valueAt20 = floatState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_20);
        assertEquals(ModelGenerator.STATE_FLOAT_TIME_20, valueAt20.getValue().getValue());

        ITimeValue<FloatValue> valueAt30 = floatState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_30);
        assertEquals(ModelGenerator.STATE_FLOAT_TIME_30, valueAt30.getValue().getValue());
    }

    @Test
    public void testIntegerState() {

        Resource myRes = ModelGenerator.createResource("@1", "Test With States", "Stated");

        IntegerTimedState integerState = myRes.getTimedState(ModelGenerator.STATE_INTEGER_ID);
        ITimeValue<IntegerValue> valueAt0 = integerState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_0);
        assertEquals(ModelGenerator.STATE_INTEGER_TIME_0, valueAt0.getValue().getValue());

        ITimeValue<IntegerValue> valueAt10 = integerState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_10);
        assertEquals(ModelGenerator.STATE_INTEGER_TIME_10, valueAt10.getValue().getValue());

        ITimeValue<IntegerValue> valueAt20 = integerState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_20);
        assertEquals(ModelGenerator.STATE_INTEGER_TIME_20, valueAt20.getValue().getValue());

        ITimeValue<IntegerValue> valueAt30 = integerState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_30);
        assertEquals(ModelGenerator.STATE_INTEGER_TIME_30, valueAt30.getValue().getValue());
    }

    @Test
    public void testBooleanState() {

        Resource myRes = ModelGenerator.createResource("@1", "Test With States", "Stated");

        BooleanTimedState booleanState = myRes.getTimedState(ModelGenerator.STATE_BOOLEAN_ID);
        ITimeValue<BooleanValue> valueAt0 = booleanState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_0);
        assertEquals(ModelGenerator.STATE_BOOLEAN_TIME_0, valueAt0.getValue().getValue());

        ITimeValue<BooleanValue> valueAt10 = booleanState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_10);
        assertEquals(ModelGenerator.STATE_BOOLEAN_TIME_10, valueAt10.getValue().getValue());

        ITimeValue<BooleanValue> valueAt20 = booleanState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_20);
        assertEquals(ModelGenerator.STATE_BOOLEAN_TIME_20, valueAt20.getValue().getValue());

        ITimeValue<BooleanValue> valueAt30 = booleanState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_30);
        assertEquals(ModelGenerator.STATE_BOOLEAN_TIME_30, valueAt30.getValue().getValue());
    }

    @Test
    public void testStringSetState() {

        Resource myRes = ModelGenerator.createResource("@1", "Test With States", "Stated");

        StringSetTimedState booleanState = myRes.getTimedState(ModelGenerator.STATE_STRING_ID);
        ITimeValue<StringSetValue> valueAt0 = booleanState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_0);
        assertEquals(asSet(ModelGenerator.STATE_STRING_TIME_0), valueAt0.getValue().getValue());

        ITimeValue<StringSetValue> valueAt10 = booleanState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_10);
        assertEquals(asSet(ModelGenerator.STATE_STRING_TIME_10), valueAt10.getValue().getValue());

        ITimeValue<StringSetValue> valueAt20 = booleanState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_20);
        assertEquals(asSet(ModelGenerator.STATE_STRING_TIME_20), valueAt20.getValue().getValue());

        ITimeValue<StringSetValue> valueAt30 = booleanState.getTimeEvolution().getValueAt(ModelGenerator.STATE_TIME_30);
        assertEquals(asSet(ModelGenerator.STATE_STRING_TIME_30), valueAt30.getValue().getValue());
    }

    private static Set<AString> asSet(String value) {
        HashSet<AString> hashSet = new HashSet<>();
        hashSet.add(new AString(value));
        return hashSet;
    }
}
