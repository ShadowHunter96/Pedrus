package cz.bbn.cerberus.commons.validator;


import com.vaadin.flow.component.Component;
import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;
import com.vaadin.flow.router.NotFoundException;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.util.Optional;

public class PhoneValidator extends AbstractValidator<String> {

    private final String errorMessage;

    public PhoneValidator() {
        super(Transl.get(TextValues.INVALID_PHONE_FORMAT));
        errorMessage = TextValues.INVALID_PHONE_FORMAT;
    }

    @Override
    public ValidationResult apply(String s, ValueContext valueContext) {
        Optional<Component> component = valueContext.getComponent();
        if (component.isEmpty()) {
            throw new NotFoundException();
        }
        if(!StringUtils.isEmpty(s) && s.replace(" ", "").length() < 9){
            return ValidationResult.error(Transl.get(errorMessage));
        }
        return ValidationResult.ok();
    }
}
