package cz.bbn.cerberus.commons.validator;

import com.vaadin.flow.data.validator.DateRangeValidator;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDate;

public class DateMinMaxValidator extends DateRangeValidator {

    public DateMinMaxValidator(LocalDate min, LocalDate max) {
        super(Transl.get("Date must be between {0} and {1}", AppUtils.formatDate(min), AppUtils.formatDate(max)),
                min, max);
    }
}
