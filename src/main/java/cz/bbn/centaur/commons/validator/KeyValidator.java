package cz.bbn.cerberus.commons.validator;

import com.vaadin.flow.data.validator.RegexpValidator;
import cz.bbn.cerberus.translation.Transl;

public class KeyValidator extends RegexpValidator {

    public KeyValidator() {
        super(Transl.get("The field can only contain big characters, numbers and an underscore"), "[A-Z0-9_]+$");
    }
}
