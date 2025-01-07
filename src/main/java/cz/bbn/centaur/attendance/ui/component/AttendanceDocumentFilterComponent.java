package cz.bbn.cerberus.attendance.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import cz.bbn.cerberus.attendance.dto.AttendanceDocumentFilterDto;
import cz.bbn.cerberus.attendance.ui.AttendanceView;
import cz.bbn.cerberus.attendance.ui.component.tab.AttendanceDocumentTabComponent;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.interfaces.FilterAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

public class AttendanceDocumentFilterComponent extends FormLayout {

    private final FilterAction filterAction;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final List<UserDto> userDtoList;
    private Map<String, String> map;

    private DatePicker createdFrom;
    private DatePicker createdTo;
    private ComboBox<UserDto> createdBy;
    private Checkbox deleted;

    public AttendanceDocumentFilterComponent(FilterAction filterAction, String params,
                                             HistoryBreadcrumbs historyBreadcrumbs,
                                             List<UserDto> userDtoList) {
        this.filterAction = filterAction;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.userDtoList = userDtoList;
        initComponent();
    }

    private void initComponent(){

        createdFrom = VaadinComponents.getDatePicker(null);
        createdFrom.setLabel(Transl.get("From"));
        createdFrom.addValueChangeListener(event -> AppUtils.fillParam(filterAction, AttendanceView.ROUTE + "/" + AttendanceDocumentTabComponent.TAB_INDEX,
                historyBreadcrumbs, map, "from", event.getValue()));
        this.add(createdFrom);

        createdTo = VaadinComponents.getDatePicker(null);
        createdTo.setLabel(Transl.get("To"));
        createdTo.addValueChangeListener(event -> AppUtils.fillParam(filterAction, AttendanceView.ROUTE + "/" + AttendanceDocumentTabComponent.TAB_INDEX,
                historyBreadcrumbs, map, "to", event.getValue()));
        this.add(createdTo);

        createdFrom.setMax(createdTo.getValue());

        createdFrom.addValueChangeListener(e -> createdTo.setMin(e.getValue()));
        createdTo.addValueChangeListener(e -> createdFrom.setMax(e.getValue()));

        createdBy = new ComboBox<>(Transl.get("Created by"));
        createdBy.setItems(userDtoList);
        createdBy.setItemLabelGenerator(UserDto::getName);
        createdBy.addValueChangeListener(event -> AppUtils.fillParam(filterAction, AttendanceView.ROUTE + "/" + AttendanceDocumentTabComponent.TAB_INDEX,
                historyBreadcrumbs, map, "createdBy", String.valueOf(event.getValue().getId())));
        this.add(createdBy);

        deleted = new Checkbox(Transl.get("Show deleted"));
        deleted.addValueChangeListener(event -> AppUtils.fillParam(filterAction, AttendanceView.ROUTE + "/" + AttendanceDocumentTabComponent.TAB_INDEX,
                historyBreadcrumbs, map, "deleted", String.valueOf(event.getValue())));
        this.add(deleted);

        fillFilterFromUrl();

        Button search = VaadinComponents.getButton(Transl.get("Search"));
        search.addClickListener(event -> filterAction.filter());
        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    private void fillFilterFromUrl() {
        map = AppUtils.getMapByParams(params);
        if (map.containsKey("from")) {
            createdFrom.setValue(LocalDate.parse(map.get("createdFrom")));
        }
        if (map.containsKey("to")) {
            createdTo.setValue(LocalDate.parse(map.get("createdTo")));
        }
        if (map.containsKey("createdBy")) {
            for (UserDto user : userDtoList) {
                if (user.getId() != null && String.valueOf(user.getId()).equals(map.get("createdBy"))) {
                    createdBy.setValue(user);
                }
            }
        }
        if (map.containsKey("deleted")) {
            deleted.setValue(Boolean.valueOf(map.get("deleted")));
        }
    }

    public AttendanceDocumentFilterDto getAttendanceDocumentFilterDto(){
        AttendanceDocumentFilterDto attendanceDocumentFilterDto = new AttendanceDocumentFilterDto();
        attendanceDocumentFilterDto.setFrom(createdFrom.getValue());
        attendanceDocumentFilterDto.setTo(createdFrom.getValue());
        attendanceDocumentFilterDto.setUserDto(createdBy.getValue());
        attendanceDocumentFilterDto.setShowDeleted(deleted.getValue());
        return attendanceDocumentFilterDto;
    }
}
