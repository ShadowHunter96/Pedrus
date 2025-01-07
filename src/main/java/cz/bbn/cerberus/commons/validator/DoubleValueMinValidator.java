package cz.bbn.cerberus.commons.validator;

import com.vaadin.flow.data.validator.DoubleRangeValidator;
import cz.bbn.cerberus.translation.Transl;

public class DoubleValueMinValidator extends DoubleRangeValidator {

    public DoubleValueMinValidator(double min) {
        super(Transl.get("Value must be greater than {0}", String.valueOf(min)), min, 999999999.0);
    }
}
