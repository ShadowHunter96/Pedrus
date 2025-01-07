package cz.bbn.cerberus.commons.validator;

import com.vaadin.flow.data.validator.StringLengthValidator;
import cz.bbn.cerberus.translation.Transl;

public class MinMaxValidator extends StringLengthValidator {

    public MinMaxValidator(Integer minLength, Integer maxLength) {
        super(Transl.get("Min length is {0} and max length is {1}",
                String.valueOf(minLength), String.valueOf(maxLength)), minLength, maxLength);
    }
}
