package cz.bbn.cerberus.employeecontract.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractFilterDto;
import cz.bbn.cerberus.employeecontract.ui.EmployeeContractView;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

public class EmployeeContractFilterDtoComponent extends FormLayout {

    private final Button searchButton;
    private final List<EmployeeDto> employeeDtoList;
    private final List<EnumerationDto> stateList;
    private final List<ContractTypeDto> typeList;
    private final List<SubjectDto> ownCompanyList;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;

    private TextField id;
    private ComboBox<EmployeeDto> employee;
    private ComboBox<EnumerationDto> state;
    private ComboBox<ContractTypeDto> type;
    private ComboBox<SubjectDto> company;
    private DatePicker validTo;
    private Checkbox showArchived;

    public EmployeeContractFilterDtoComponent(Button searchButton, List<EmployeeDto> employeeDtoList,
                                              List<EnumerationDto> stateList, List<ContractTypeDto> typeList,
                                              List<SubjectDto> ownCompanyList, String params,
                                              HistoryBreadcrumbs historyBreadcrumbs) {
        this.searchButton = searchButton;
        this.employeeDtoList = employeeDtoList;
        this.stateList = stateList;
        this.typeList = typeList;
        this.ownCompanyList = ownCompanyList;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        initFilter();
    }

    private void initFilter() {
        id = new TextField(Transl.get("Contract number"));
        add(id);

        employee = new ComboBox<>(Transl.get("Employee"));
        employee.setItems(employeeDtoList);
        employee.setItemLabelGenerator(this::getEmpName);
        add(employee);

        state = new ComboBox<>(Transl.get("State"));
        state.setItems(stateList);
        state.setItemLabelGenerator(EnumerationDto::getName);
        add(state);

        type = new ComboBox<>(Transl.get("Type"));
        type.setItems(typeList);
        type.setItemLabelGenerator(ContractTypeDto::getName);
        add(type);

        company = new ComboBox<>(Transl.get("Company"));
        company.setItems(ownCompanyList);
        company.setItemLabelGenerator(SubjectDto::getName);
        add(company);

        validTo = VaadinComponents.getDatePicker(Transl.get("Valid to"), null);
        add(validTo);

        showArchived = new Checkbox(Transl.get("Show archived"));
        add(showArchived);

        add(searchButton);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);

        fillFilterFromUrl();
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("id")) {
            id.setValue(map.get("id"));
        }

        if (map.containsKey("employee")) {
            for (EmployeeDto employeeDto : employeeDtoList) {
                if (employeeDto.getId().equals(map.get("employee"))) {
                    employee.setValue(employeeDto);
                }
            }
        }

        if (map.containsKey("state")) {
            for (EnumerationDto enumerationDto : stateList) {
                if (String.valueOf(enumerationDto.getId()).equals(map.get("state"))) {
                    state.setValue(enumerationDto);
                }
            }
        }

        if (map.containsKey("type")) {
            for (ContractTypeDto typePar : typeList) {
                if (typePar.getId().equals(map.get("type"))) {
                    type.setValue(typePar);
                }
            }
        }

        if (map.containsKey("company")) {
            for (SubjectDto subjectDto : ownCompanyList) {
                if (subjectDto.getId().equals(map.get("company"))) {
                    company.setValue(subjectDto);
                }
            }
        }

        if (map.containsKey("validTo")) {
            validTo.setValue(LocalDate.parse(map.get("validTo")));
        }

        if (map.containsKey("showArchived") &&
                SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_SHOW_ARCHIVED)) {
            showArchived.setValue("true".equalsIgnoreCase(map.get("showArchived")));
        }
    }

    public String getParamUrl(){
        String paramUrl = EmployeeContractView.ROUTE.concat("/");

        if (id.getValue() != null && !id.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&id=").concat(id.getValue());
        }

        if (employee.getValue() != null) {
            paramUrl = paramUrl.concat("&employee=").concat(employee.getValue().getId());
        }

        if (state.getValue() != null) {
            paramUrl = paramUrl.concat("&state=").concat(String.valueOf(state.getValue().getId()));
        }

        if (type.getValue() != null) {
            paramUrl = paramUrl.concat("&type=").concat(type.getValue().getId());
        }

        if (company.getValue() != null) {
            paramUrl = paramUrl.concat("&company=").concat(company.getValue().getId());
        }

        if (validTo.getValue() != null) {
            paramUrl = paramUrl.concat("&validTo=").concat(validTo.getValue().toString());
        }

        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_SHOW_ARCHIVED)) {
            paramUrl = paramUrl.concat("&showArchived=")
                    .concat(String.valueOf(Boolean.TRUE.equals(showArchived.getValue())));
        }
        return paramUrl;
    }

    public void fillUrl() {
        String paramUrl = getParamUrl();
        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public EmployeeContractFilterDto getEmployeeContractFilterDto() {
        EmployeeContractFilterDto employeeContractFilterDto = new EmployeeContractFilterDto();
        employeeContractFilterDto.setId(id.getValue());
        employeeContractFilterDto.setEmployee(employee.getValue());
        employeeContractFilterDto.setState(state.getValue());
        employeeContractFilterDto.setType(type.getValue());
        employeeContractFilterDto.setValidTo(validTo.getValue());
        employeeContractFilterDto.setCompany(company.getValue());
        employeeContractFilterDto.setShowDeleted(false);
        employeeContractFilterDto.setShowArchived(showArchived.getValue());
        return employeeContractFilterDto;
    }

    private String getEmpName(EmployeeDto empDto) {
        if (empDto != null) {
            return empDto.getFirstName() + " " + empDto.getLastName();
        }
        return "";
    }

}
