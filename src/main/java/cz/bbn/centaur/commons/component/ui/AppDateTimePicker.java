package cz.bbn.cerberus.commons.component.ui;

import com.vaadin.flow.component.datetimepicker.DateTimePicker;
import com.vaadin.flow.component.dependency.CssImport;
import com.vaadin.flow.router.Route;

import java.time.LocalDateTime;

@Route("datetimepicker-weekend-highlight")
@CssImport(value = "./styles/highlightweekend.css", themeFor = "vaadin-month-calendar")
public class AppDateTimePicker extends DateTimePicker {

    public AppDateTimePicker(String label, LocalDateTime initialDate) {
        super(label, initialDate);
    }
}
