package cz.bbn.cerberus.commons.validator;

import com.vaadin.flow.data.validator.IntegerRangeValidator;
import cz.bbn.cerberus.translation.Transl;

public class IntegerValueMinMaxValidator extends IntegerRangeValidator {

    public IntegerValueMinMaxValidator(int min, int max) {
        super(Transl.get("Value must be between {0} and {1}", String.valueOf(min), String.valueOf(max)), min, max);
    }
}
