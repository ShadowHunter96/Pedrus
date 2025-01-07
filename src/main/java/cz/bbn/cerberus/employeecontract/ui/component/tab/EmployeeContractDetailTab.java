package cz.bbn.cerberus.employeecontract.ui.component.tab;

import com.vaadin.componentfactory.TooltipAlignment;
import com.vaadin.componentfactory.TooltipPosition;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppTooltip;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.tab.TabDtoComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.WarningNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractEndingDays;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employeecontract.EmployeeContractComponentOperation;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.ui.EmployeeContractDetailView;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@Slf4j
public class EmployeeContractDetailTab extends TabDtoComponent<EmployeeContractDto> {

    private final EmployeeContractComponentOperation componentOperation;

    private final List<SubjectDto> ownCompanyList;
    private final List<ContractTypeDto> typeList;
    private final List<EmployeeDto> employeeList;
    private final List<EmployeeContractDto> empContractList;
    private final List<EnumerationDto> stateList;

    private ComboBox<EmployeeContractDto> linkedContract;

    public EmployeeContractDetailTab(EmployeeContractDto dto, SaveAction<EmployeeContractDto> saveAction,
                                     List<SubjectDto> ownCompanyList, List<ContractTypeDto> typeList,
                                     List<EmployeeDto> employeeList, List<EmployeeContractDto> empContractList,
                                     EmployeeContractComponentOperation componentOperation,
                                     List<EnumerationDto> stateList, AppEnv appEnv) {
        super(dto, saveAction, appEnv);
        this.ownCompanyList = ownCompanyList;
        this.typeList = typeList;
        this.employeeList = employeeList;
        this.empContractList = empContractList;
        this.componentOperation = componentOperation;
        this.stateList = stateList;
        initTab();
    }

    @Override
    public void initTab() {

        boolean canEdit = SecurityUtils.hasPermission(Permission.EMP_CONTRACT_STATE_EDIT);
        boolean isNew = getBinder().getBean().getId() == null;

        FormLayout formLayout = new FormLayout();

        TextField id = new TextField(Transl.get("Id"));
        id.setReadOnly(true);
        getBinder().forField(id).bind(EmployeeContractDto::getId, EmployeeContractDto::setId);
        formLayout.add(id);
        id.setVisible(!isNew);

        ComboBox<SubjectDto> ownCompany = new ComboBox<>(Transl.get("Own company"));
        ownCompany.setItems(ownCompanyList);
        ownCompany.setItemLabelGenerator(SubjectDto::getName);
        getBinder().forField(ownCompany).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmployeeContractDto::getOwnCompany, EmployeeContractDto::setOwnCompany);
        formLayout.add(ownCompany);

        ComboBox<ContractTypeDto> type = new ComboBox<>(Transl.get("Type"));
        type.setItems(typeList);
        type.setItemLabelGenerator(ContractTypeDto::getName);
        getBinder().forField(type).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmployeeContractDto::getType, EmployeeContractDto::setType);
        formLayout.add(type);

        ComboBox<EmployeeDto> employee = new ComboBox<>(Transl.get("Employee"));
        employee.setItems(employeeList);
        employee.setItemLabelGenerator(emp -> emp.getFirstName() + " " + emp.getLastName());
        getBinder().forField(employee).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmployeeContractDto::getEmployee, EmployeeContractDto::setEmployee);
        formLayout.add(employee);

        AppTooltip contractTooltip = new AppTooltip();
        contractTooltip.setAlignment(TooltipAlignment.BOTTOM);
        contractTooltip.setPosition(TooltipPosition.BOTTOM);
        this.add(contractTooltip);

        HorizontalLayout contractLayout = new HorizontalLayout();
        contractLayout.setAlignItems(FlexComponent.Alignment.END);
        linkedContract = new ComboBox<>(Transl.get("Connected contract"));
        if (employee.getValue() != null) {
            linkedContract.setItems(filterEmpContract(empContractList, employee.getValue()));
        } else {
            linkedContract.setItems(new ArrayList<>());
        }
        getBinder().forField(linkedContract).bind(
                this::getEmployeeContract, (empCon, linkedEmp) -> {
                    if (linkedEmp != null) {
                        empCon.setLinkedContractId(linkedEmp.getId());
                    }
                });
        linkedContract.setItemLabelGenerator(EmployeeContractDto::getName);
        contractLayout.add(linkedContract);

        employee.addValueChangeListener(e -> {
            if (e.getValue() != null) {
                linkedContract.setItems(filterEmpContract(empContractList, e.getValue()));
            }
        });

        if (getBinder().getBean().getLinkedContractId() != null) {
            linkedContract.setReadOnly(true);
        }

        Button viewButton = VaadinComponents.getEyeButton();
        linkedContract.addValueChangeListener(e -> {
            setTooltipText(contractTooltip, e.getValue());
            if (e.getValue() != null) {
                viewButton.removeClassName("disabled-color");
                viewButton.setEnabled(true);
            } else {
                viewButton.addClassName("disabled-color");
                viewButton.setEnabled(false);
            }
        });
        setTooltipText(contractTooltip, linkedContract.getValue());

        viewButton.setTabIndex(-1);
        if (linkedContract.getValue() == null) {
            viewButton.addClassName("disabled-color");
            viewButton.setEnabled(false);
        }
        viewButton.addClickListener(buttonClickEvent -> {
            if (linkedContract.getValue() != null) {
                UI.getCurrent().navigate(EmployeeContractDetailView.ROUTE + "/" + linkedContract.getValue().getId());
            } else {
                WarningNotification.show(Transl.get("No contract is selected"), getApptEnv());
            }

        });
        contractLayout.add(viewButton);
        contractTooltip.attachToComponent(viewButton);
        formLayout.add(contractLayout);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmployeeContractDto::getName, EmployeeContractDto::setName);
        formLayout.add(name);

        DatePicker validFrom = VaadinComponents.getDatePicker(Transl.get("Valid from"), null);
        getBinder().forField(validFrom).bind(EmployeeContractDto::getValidFrom, EmployeeContractDto::setValidFrom);
        formLayout.add(validFrom);

        DatePicker validTo = VaadinComponents.getDatePicker(Transl.get("Valid to"), null);
        getBinder().forField(validTo).bind(EmployeeContractDto::getValidTo, EmployeeContractDto::setValidTo);
        formLayout.add(validTo);

        validFrom.setMax(validTo.getValue());
        validTo.setMin(validFrom.getValue());

        validFrom.addValueChangeListener(e -> validTo.setMin(e.getValue()));

        validTo.addValueChangeListener(e -> validFrom.setMax(e.getValue()));

        ComboBox<ContractEndingDays> reminder = new ComboBox<>(Transl.get("Reminder"));
        reminder.setItems(ContractEndingDays.values());
        reminder.setItemLabelGenerator(conD -> String.valueOf(conD.getDays()));
        getBinder().forField(reminder).bind(this::getEndingDay, this::setReminder);
        formLayout.add(reminder);

        TextField contractNumber = new TextField(Transl.get("Contract number"));
        contractNumber.setMaxLength(100);
        getBinder().forField(contractNumber)
                .bind(EmployeeContractDto::getContractNumber, EmployeeContractDto::setContractNumber);
        formLayout.add(contractNumber);

        ComboBox<EnumerationDto> state = new ComboBox<>(Transl.get("State"));
        state.setItems(stateList);
        state.setItemLabelGenerator(EnumerationDto::getName);
        getBinder().forField(state).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmployeeContractDto::getState, EmployeeContractDto::setState);
        formLayout.add(state);

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(255);
        getBinder().forField(description)
                .bind(EmployeeContractDto::getDescription, EmployeeContractDto::setDescription);
        description.setWidthFull();
        description.setMinHeight("15em");

        add(formLayout);
        add(description);

        if (!canEdit) {
            ownCompany.setReadOnly(true);
            employee.setReadOnly(true);
            linkedContract.setReadOnly(true);
            name.setReadOnly(true);
            validFrom.setReadOnly(true);
            validTo.setReadOnly(true);
            reminder.setReadOnly(true);
            contractNumber.setReadOnly(true);
            state.setReadOnly(true);
            description.setReadOnly(true);
        }

        if (!isNew) {
            ownCompany.setReadOnly(true);
            employee.setReadOnly(true);
            linkedContract.setReadOnly(true);
        }

        type.addValueChangeListener(e -> {
            if (e.getValue() != null && Boolean.TRUE.equals(e.getValue().getConnectionRequired())) {
                linkedContract.setRequired(true);
                linkedContract.setErrorMessage(Transl.get(TextValues.CANNOT_BE_EMPTY));
            } else {
                linkedContract.setRequired(false);
            }
        });

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
    }

    private EmployeeContractDto getEmployeeContract(EmployeeContractDto empCon) {
        if (empCon.getLinkedContractId() != null) {
            try {
                return componentOperation.getEmployeeContract(empCon.getLinkedContractId());
            } catch (SystemException e) {
                log.error(e.getMessage(), e);
                ErrorNotification.show(e.getMessage(), getApptEnv());
            }
        }
        return null;
    }

    private ContractEndingDays getEndingDay(EmployeeContractDto dto) {
        return ContractEndingDays.getByDays(dto.getReminder());
    }

    private void setReminder(EmployeeContractDto empCon, ContractEndingDays rem) {
        empCon.setReminder(rem.getDays());
    }

    private void setTooltipText(AppTooltip appTooltip, EmployeeContractDto contractDto) {
        appTooltip.removeAll();
        VerticalLayout verticalLayout = new VerticalLayout();
        if (contractDto != null) {
            verticalLayout.add(new Label(Transl.get("Name") + ": " + contractDto.getName()));
            appTooltip.add(verticalLayout);
        } else {
            appTooltip.add(new Label(Transl.get("No contract selected")));
        }
    }

    private List<EmployeeContractDto> filterEmpContract(List<EmployeeContractDto> empContractList,
                                                        EmployeeDto employeeDto) {
        List<EmployeeContractDto> employeeContractDtoList = new ArrayList<>();
        for (EmployeeContractDto employeeContractDto : empContractList) {
            if (employeeDto != null && employeeContractDto.getEmployee() != null
                    && employeeDto.getId().equals(employeeContractDto.getEmployee().getId())) {
                employeeContractDtoList.add(employeeContractDto);
            }
        }
        return employeeContractDtoList;
    }
}
