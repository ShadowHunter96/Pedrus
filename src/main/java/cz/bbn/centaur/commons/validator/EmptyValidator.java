package cz.bbn.cerberus.commons.validator;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;
import org.apache.commons.lang3.StringUtils;

public class EmptyValidator extends AbstractValidator<String> {

    private final String errorMessage;

    public EmptyValidator(String errorMessage) {
        super(errorMessage);
        this.errorMessage = errorMessage;
    }

    @Override
    public ValidationResult apply(String s, ValueContext valueContext) {
        if(StringUtils.isEmpty(s)){
            return ValidationResult.error(errorMessage);
        }
        return ValidationResult.ok();
    }
}
