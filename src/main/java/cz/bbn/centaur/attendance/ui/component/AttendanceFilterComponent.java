package cz.bbn.cerberus.attendance.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import cz.bbn.cerberus.attendance.dto.AttendanceFilterDto;
import cz.bbn.cerberus.attendance.ui.AttendanceView;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.interfaces.FilterAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;

import java.util.List;
import java.util.Map;

public class AttendanceFilterComponent extends FormLayout {

    private final FilterAction filterAction;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final List<YearMonthDto> yearMonthList;

    private ComboBox<YearMonthDto> yearMonthComboBox;
    private Map<String, String> map;

    public AttendanceFilterComponent(FilterAction filterAction, String params,
                                     HistoryBreadcrumbs historyBreadcrumbs,
                                     ComboBox<YearMonthDto> yearMonthComboBox) {
        this.filterAction = filterAction;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        yearMonthList = AppUtils.getYearMonthList();
        this.yearMonthComboBox = yearMonthComboBox;
        initComponent();
    }

    private void initComponent(){
        yearMonthComboBox.setItems(yearMonthList);
        yearMonthComboBox.setItemLabelGenerator(this::getName);
        if (!yearMonthList.isEmpty()) {
            yearMonthComboBox.setValue(yearMonthList.get(0));
        }
        yearMonthComboBox.addValueChangeListener(event -> {
            if(event.isFromClient() && event.getValue() != null) {
                AppUtils.fillParam(filterAction, AttendanceView.ROUTE, historyBreadcrumbs, map, "month", event.getValue().getYearAndMonth());
            }
        });
        this.add(yearMonthComboBox);

        fillFilterFromUrl();

        Button search = VaadinComponents.getButton(Transl.get("Search"));
        search.addClickListener(event -> {
            AppUtils.fillParam(filterAction, AttendanceView.ROUTE, historyBreadcrumbs, map, "month", yearMonthComboBox.getValue().getYearAndMonth());
            filterAction.filter();
        });
        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    private void fillFilterFromUrl() {
        map = AppUtils.getMapByParams(params);

        if (map.containsKey("month")) {
            String[] value = map.get("month").split(",");
            YearMonthDto yearMonthDto = yearMonthList.stream().filter(actualMonth -> actualMonth.equals(new YearMonthDto(Integer.valueOf(value[0]), value[1]))).findAny().orElse(null);
            if(yearMonthDto != null) {
                yearMonthComboBox.setValue(yearMonthDto);
            }
        }
    }

    public AttendanceFilterDto getAttendanceFilterDto(){
        AttendanceFilterDto attendanceFilterDto = new AttendanceFilterDto();
        attendanceFilterDto.setYear(yearMonthComboBox.getValue().getDate().getYear());
        attendanceFilterDto.setMonth(yearMonthComboBox.getValue().getDate().getMonth().getValue());
        return attendanceFilterDto;
    }

    private String getName(YearMonthDto yearMonthDto) {
        return Transl.get(yearMonthDto.getMonth()) + " " + yearMonthDto.getYear();
    }
}
