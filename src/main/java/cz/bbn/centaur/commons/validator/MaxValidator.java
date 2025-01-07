package cz.bbn.cerberus.commons.validator;

import com.vaadin.flow.data.validator.StringLengthValidator;
import cz.bbn.cerberus.translation.Transl;

public class MaxValidator extends StringLengthValidator {

    public MaxValidator(int length) {
        super(Transl.get("Max length of value is ").concat(String.valueOf(length)), 0, length);
    }
}
