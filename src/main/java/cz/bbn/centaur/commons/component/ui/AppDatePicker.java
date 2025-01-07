package cz.bbn.cerberus.commons.component.ui;

import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.dependency.CssImport;
import com.vaadin.flow.router.Route;

import java.time.LocalDate;

@Route("datepicker-weekend-highlight")
@CssImport(value = "./styles/highlightweekend.css", themeFor = "vaadin-month-calendar")
public class AppDatePicker extends DatePicker {

    public AppDatePicker(String label, LocalDate initialDate) {
        super(label, initialDate);
    }

    public AppDatePicker(LocalDate initialDate) {
        super(initialDate);
    }
}
