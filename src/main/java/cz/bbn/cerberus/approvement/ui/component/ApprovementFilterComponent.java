package cz.bbn.cerberus.approvement.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import cz.bbn.cerberus.approvement.dto.ApprovementFilterDto;
import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.interfaces.CountActionDouble;
import cz.bbn.cerberus.commons.component.ui.interfaces.FilterAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ApprovementFilterComponent extends FormLayout {

    private final List<EmployeeDto> employeeDtoList;
    private final FilterAction filterAction;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final ApprovementType approvementType;
    private final EmployeeDto employeeDto;
    private final CountActionDouble countActionDouble;
    private final ComboBox<Integer> year;
    private final boolean master;
    private final boolean approvement;
    private final boolean backOffice;
    private final String route;

    private IntegerField id;
    private DatePicker dateFrom;
    private DatePicker dateTo;

    private MultiSelectComboBox<EmployeeDto> employee;
    private MultiSelectComboBox<ApprovementType> type;
    private MultiSelectComboBox<ApprovementState> state;
    private Map<String, String> map;

    public ApprovementFilterComponent(String params, List<EmployeeDto> employeeDtoList, FilterAction filterAction,
                                      HistoryBreadcrumbs historyBreadcrumbs, ComboBox<Integer> year, String route,
                                      boolean master) {
        this.params = params;
        this.employeeDtoList = employeeDtoList;
        this.filterAction = filterAction;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.year = year;
        this.route = route;
        this.approvementType = null;
        this.employeeDto = null;
        this.countActionDouble = null;
        this.master = master;
        this.approvement = true;
        this.backOffice = false;
        initComponent();
    }

    public ApprovementFilterComponent(String params, List<EmployeeDto> employeeDtoList, FilterAction filterAction,
                                      HistoryBreadcrumbs historyBreadcrumbs, ApprovementType approvementType,
                                      EmployeeDto employeeDto, CountActionDouble countActionDouble,
                                      ComboBox<Integer> year, String route, boolean backOffice, boolean master) {
        this.params = params;
        this.employeeDtoList = employeeDtoList;
        this.filterAction = filterAction;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.approvementType = approvementType;
        this.employeeDto = master ? null : employeeDto;
        this.countActionDouble = countActionDouble;
        this.year = year;
        this.route = route;
        this.master = master;
        this.backOffice = backOffice;
        this.approvement = false;
        initComponent();
    }

    private void initComponent() {
        id = new IntegerField(Transl.get("id"));
        id.addValueChangeListener(event -> {
            if (event.isFromClient() && event.getValue() != null) {
                AppUtils.fillParam(filterAction, route, historyBreadcrumbs, map, "id", id.getValue().toString());
            }
        });
        this.add(id);
        year.addValueChangeListener(event -> {
            if (event.isFromClient() && event.getValue() != null) {
                AppUtils.fillParam(filterAction, route, historyBreadcrumbs, map, "year", year.getValue().toString());
                if (countActionDouble != null) {
                    countActionDouble.getCount();
                }
            }
        });
        this.add(year);

        dateFrom = VaadinComponents.getDatePicker(Transl.get("From"), null);
        dateFrom.addValueChangeListener(event -> {
            if (event.isFromClient() && event.getValue() != null) {
                AppUtils.fillParam(filterAction, route, historyBreadcrumbs, map, "dateFrom", dateFrom.getValue());
            }
        });
        this.add(dateFrom);

        dateTo = VaadinComponents.getDatePicker(Transl.get("to"), null);
        dateTo.addValueChangeListener(event -> {
            if (event.isFromClient() && event.getValue() != null) {
                AppUtils.fillParam(filterAction, route, historyBreadcrumbs, map, "dateTo", dateTo.getValue());
            }
        });
        this.add(dateTo);

        employee = new MultiSelectComboBox<>(Transl.get("Employee"));
        employee.setItems(employeeDtoList);
        employee.setItemLabelGenerator(actualEmployeeDto -> actualEmployeeDto.getFirstName().concat(" ")
                .concat(actualEmployeeDto.getLastName()));
        employee.addValueChangeListener(event -> {
            if (event.isFromClient() && event.getValue() != null) {
                AppUtils.fillParam(filterAction, route, historyBreadcrumbs, map, "employee",
                        StringUtils.join(employee.getValue().stream()
                                .map(EmployeeDto::getId)
                                .toList(), ","));
            }
        });

        type = new MultiSelectComboBox<>(Transl.get("Type"));
        type.setItems(ApprovementType.values());
        type.setItemLabelGenerator(actualApprovementType -> Transl.get(actualApprovementType.name()));
        type.addValueChangeListener(event -> {
            if (event.isFromClient() && event.getValue() != null) {
                AppUtils.fillParam(filterAction, route, historyBreadcrumbs, map, "type",
                        StringUtils.join(type.getValue().stream().toList(), ","));
            }
        });

        state = new MultiSelectComboBox<>(Transl.get("State"));
        state.setItems(ApprovementState.values());
        state.setItemLabelGenerator(approvementState -> Transl.get(approvementState.name()));
        state.addValueChangeListener(event -> {
            if (event.isFromClient() && event.getValue() != null) {
                AppUtils.fillParam(filterAction, route, historyBreadcrumbs, map, "state",
                        StringUtils.join(state.getValue().stream().toList(), ","));
            }
        });

        fillFilterFromUrl();

        if (approvementType == null) {
            this.add(type, employee);
        } else {
            type.setValue(approvementType);
            if (!backOffice && employeeDto != null) {
                employee.setValue(employeeDto);
            }
        }
        this.add(state);

        Button search = VaadinComponents.getButton(Transl.get("Search"));
        search.addClickListener(event -> {
            filterAction.filter();
            if (countActionDouble != null) {
                countActionDouble.getCount();
            }
        });
        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    private void fillFilterFromUrl() {
        map = AppUtils.getMapByParams(params);

        if (map.containsKey("id")) {
            id.setValue(Integer.valueOf(map.get("id")));
        }

        if (map.containsKey("year")) {
            year.setValue(Integer.valueOf(map.get("year")));
        }

        if (map.containsKey("dateFrom")) {
            dateFrom.setValue(LocalDate.parse(map.get("dateFrom")));
        }

        if (map.containsKey("dateTo")) {
            dateFrom.setValue(LocalDate.parse(map.get("dateTo")));
        }

        if (map.containsKey("state")) {
            String[] actualState = map.get("state").split(",");
            Set<ApprovementState> approvementStateSet = new HashSet<>();
            Arrays.stream(actualState).forEach(s -> approvementStateSet.add(ApprovementState.valueOf(s)));
            state.setValue(approvementStateSet);
        }

        if (map.containsKey("type")) {
            String[] actualType = map.get("type").split(",");
            Set<ApprovementType> approvementTypeSet = new HashSet<>();
            Arrays.stream(actualType).forEach(s -> approvementTypeSet.add(ApprovementType.valueOf(s)));
            type.setValue(approvementTypeSet);
        }

        if (map.containsKey("employee")) {
            String[] actualEmployees = map.get("employee").split(",");
            Set<EmployeeDto> employeeDtoSet = new HashSet<>();
            Arrays.stream(actualEmployees).forEach(s ->
                    employeeDtoSet.add(employeeDtoList.stream().filter(actualEmployeeDto ->
                            actualEmployeeDto.getId().equals(s)).findAny().get()));
            employee.setValue(employeeDtoSet);
        }
    }

    public ApprovementFilterDto getApprovementFilterDto() {
        if (year.getValue() == null) {
            year.setValue(LocalDate.now().getYear());
        }
        ApprovementFilterDto approvementFilterDto = new ApprovementFilterDto();
        approvementFilterDto.setId(id.getValue());
        approvementFilterDto.setDateFrom(dateFrom.getValue());
        approvementFilterDto.setDateTo(dateTo.getValue());
        approvementFilterDto.setMaster(master);
        approvementFilterDto.setApprovement(approvement);
        approvementFilterDto.setEmployeeDtoSet(employee.getSelectedItems());
        approvementFilterDto.setStateSet(state.getSelectedItems());
        approvementFilterDto.setTypeSet(type.getSelectedItems());
        approvementFilterDto.setYear(year.getValue());
        return approvementFilterDto;
    }
}
