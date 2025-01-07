package cz.bbn.cerberus.commons.validator;

import com.vaadin.flow.data.validator.RegexpValidator;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.translation.Transl;

public class EmailValidator extends RegexpValidator {

    public EmailValidator() {
        super(Transl.get(TextValues.INVALID_EMAIL_FORMAT),
                "^(?=.{1,255}@)[A-Za-z0-9_-]+(\\.[A-Za-z0-9_-]+)*@"
                        + "[^-][A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*(\\.[A-Za-z]{2,})$");
    }
}
