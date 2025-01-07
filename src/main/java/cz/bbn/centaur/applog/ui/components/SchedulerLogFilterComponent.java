package cz.bbn.cerberus.applog.ui.components;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.schedulerlog.dto.SchedulerLogFilterDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class SchedulerLogFilterComponent extends FormLayout {

    private DatePicker date;
    private ComboBox<String> description;

    private final Button search;
    private final List<String> descriptionList;

    public SchedulerLogFilterComponent(Button search, List<String> descriptionList) {
        this.search = search;
        this.descriptionList = descriptionList;
        initComponent();
    }

    private void initComponent() {
        description = new ComboBox<>(Transl.get("Descriptions"));
        description.setItems(descriptionList);
        this.add(description);

        date = VaadinComponents.getDatePicker(null);
        date.setLabel(Transl.get("Date"));
        this.add(date);
        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public SchedulerLogFilterDto getSchedulerLogFilterDto() {
        SchedulerLogFilterDto schedulerLogFilterDto = new SchedulerLogFilterDto();
        schedulerLogFilterDto.setDate(date.getValue());
        return schedulerLogFilterDto;
    }
}
