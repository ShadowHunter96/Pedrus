package cz.bbn.cerberus.commons.validator;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasEnabled;
import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;
import com.vaadin.flow.router.NotFoundException;
import org.apache.commons.lang3.StringUtils;

import java.util.Optional;

public class EnabledValidator extends AbstractValidator<String> {

    private final String errorMessage;

    public EnabledValidator(String errorMessage) {
        super(errorMessage);
        this.errorMessage = errorMessage;
    }

    @Override
    public ValidationResult apply(String s, ValueContext valueContext) {

        Optional<Component> component = valueContext.getComponent();
        if (component.isEmpty()) {
            throw new NotFoundException();
        }
        if (((HasEnabled) component.get()).isEnabled() && StringUtils.isEmpty(s)) {
            return ValidationResult.error(errorMessage);
        }
        return ValidationResult.ok();
    }

}
