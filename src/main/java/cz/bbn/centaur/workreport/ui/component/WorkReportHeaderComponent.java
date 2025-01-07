package cz.bbn.cerberus.workreport.ui.component;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;

import java.util.List;

public class WorkReportHeaderComponent extends HorizontalLayout {

    private final List<YearMonthDto> yearMonthList;
    private final WorkReportListener listener;
    private final Div hourDiv = new Div();

    public WorkReportHeaderComponent(WorkReportListener listener) {
        this.listener = listener;
        yearMonthList = AppUtils.getYearMonthList();
        initComponent();
    }

    private void initComponent() {
        setWidthFull();
        H2 heading = new H2(Transl.get("Work report"));
        heading.setMinWidth("9.15em");
        this.add(heading);

        ComboBox<YearMonthDto> yearMonthComboBox = new ComboBox<>(Transl.get("Month"));
        yearMonthComboBox.setItems(yearMonthList);
        yearMonthComboBox.setItemLabelGenerator(this::getName);
        if (!yearMonthList.isEmpty()) {
            yearMonthComboBox.setValue(yearMonthList.get(0));
        }
        this.add(yearMonthComboBox);
        this.add(hourDiv);
        hourDiv.setWidthFull();
        hourDiv.getElement().getStyle().set("text-align", "end").set("margin-right", "1em");

        yearMonthComboBox.addValueChangeListener(e -> listener.changeYearMonthValue(e.getValue()));
    }

    public void setHourText(String text, String color) {
        hourDiv.removeAll();
        H2 hourLabel = new H2(text);
        hourLabel.getElement().getStyle().set("color", color);
        hourDiv.add(hourLabel);
    }

    private String getName(YearMonthDto yearMonthDto) {
        return Transl.get(yearMonthDto.getMonth()) + " " + yearMonthDto.getYear();
    }
}
