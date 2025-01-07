package cz.bbn.cerberus.approvement.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.datetimepicker.DateTimePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.NumberField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationResult;
import cz.bbn.cerberus.approvement.dto.ApprovementDto;
import cz.bbn.cerberus.approvement.dto.ApprovementProjectEmployeeDto;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.approvement.enums.BusinessTripTransportationType;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.time.LocalTime;
import java.util.List;
import java.util.stream.Collectors;

public class ApprovementDetailComponent extends VerticalLayout {

    private final AppBinderOperation<ApprovementDto> appBinderOperation;
    private final boolean canEditBasicFields;
    private final boolean lineManagerCanEdit;
    private final boolean superiorCanEdit;
    private final List<ProjectDto> projectDtoList;
    private final List<EmployeeDto> employeeDtoList;
    private final ApprovementDto dto;
    private final List<HolidayEntity> holidayEntityList;
    private final List<SubjectDto> subjectDtoList;
    private final List<OpportunityDto> opportunityDtoList;
    private final EnumerationComponentOperation enumerationComponentOperation;
    private final EmployeeDto userEmployeeDto;

    private HorizontalLayout projectLayout;
    private boolean master;

    public ApprovementDetailComponent(AppBinderOperation<ApprovementDto> appBinderOperation,
                                      boolean canEditCanEditBasicFields, boolean lineManagerCanEdit,
                                      boolean superiorCanEdit, List<ProjectDto> projectDtoList,
                                      List<EmployeeDto> employeeDtoList, List<HolidayEntity> holidayEntityList,
                                      List<SubjectDto> subjectDtoList, List<OpportunityDto> opportunityDtoList,
                                      EnumerationComponentOperation enumerationComponentOperation,
                                      EmployeeDto userEmployeeDto, boolean master) {
        this.appBinderOperation = appBinderOperation;
        this.canEditBasicFields = canEditCanEditBasicFields;
        this.lineManagerCanEdit = lineManagerCanEdit;
        this.superiorCanEdit = superiorCanEdit;
        this.projectDtoList = projectDtoList;
        this.dto = appBinderOperation.getDto();
        this.employeeDtoList = employeeDtoList;
        this.holidayEntityList = holidayEntityList;
        this.subjectDtoList = subjectDtoList;
        this.opportunityDtoList = opportunityDtoList;
        this.enumerationComponentOperation = enumerationComponentOperation;
        this.userEmployeeDto = userEmployeeDto;
        this.master = master;
        initComponent();
    }

    private void initComponent() {
        FormLayout formLayout = new FormLayout();
        if (master && !dto.getApprovementType().equals(ApprovementType.ILL)) {
            ComboBox<EmployeeDto> employee = new ComboBox<>(Transl.get("Employee"));
            employee.setItems(employeeDtoList);
            employee.setItemLabelGenerator(employeeDto ->
                    employeeDto.getFirstName().concat(" ").concat(employeeDto.getLastName()));
            appBinderOperation.getBinder().forField(employee).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(ApprovementDto::getCreatedForEmployeeDto, ApprovementDto::setCreatedForEmployeeDto);
            employee.setReadOnly(dto.getId() != null);
            formLayout.add(employee);
        }
        NumberField days = new NumberField(Transl.get("Work days"));
        appBinderOperation.getBinder().forField(days).bind(ApprovementDto::getDays, ApprovementDto::setDays);

        DateTimePicker interruptionFrom = VaadinComponents.getDateTimePicker(Transl.get("Interruption from"), null);
        DateTimePicker interruptionTo = VaadinComponents.getDateTimePicker(Transl.get("Interruption to"), null);
        Checkbox halfDay = new Checkbox(Transl.get("Half day"));
        DatePicker dateFrom = VaadinComponents.getDatePicker(Transl.get("From"),
                dto.getDateFrom());
        DatePicker dateTo = VaadinComponents.getDatePicker(Transl.get("To"),
                dto.getDateTo());
        dateFrom.addValueChangeListener(event -> {
            if (event.getValue() != null) {
                if (halfDay.getValue()) {
                    dateTo.setValue(event.getValue());
                    dto.setDays(0.5);
                    days.setValue(0.5);
                } else {
                    double workDays = AppUtils.countWorkDays(event.getValue(), dateTo.getValue(), holidayEntityList);
                    dateTo.setMin(event.getValue());
                    dto.setDays(workDays);
                    days.setValue(workDays);
                }
                interruptionFrom.setMin(event.getValue().atStartOfDay());
                interruptionTo.setMin(event.getValue().atStartOfDay());
            }else{
                interruptionFrom.setMin(null);
                interruptionTo.setMin(null);
            }
            interruptionFrom.setValue(null);
            interruptionTo.setValue(null);
        });
        appBinderOperation.getBinder().forField(dateFrom).asRequired(
                Transl.get(TextValues.CANNOT_BE_EMPTY)).bind(ApprovementDto::getDateFrom, ApprovementDto::setDateFrom);
        formLayout.add(dateFrom);

        dateTo.addValueChangeListener(event -> {
            if (event.getValue() != null) {
                double workDays = AppUtils.countWorkDays(dateFrom.getValue(), event.getValue(), holidayEntityList);
                if (!Boolean.TRUE.equals(halfDay.getValue())) {
                    dateFrom.setMax(event.getValue());
                }
                dto.setDays(workDays);
                days.setValue(workDays);
                interruptionFrom.setMax(event.getValue().atTime(LocalTime.MAX));
                interruptionTo.setMax(event.getValue().atTime(LocalTime.MAX));
            }else{
                interruptionFrom.setMax(null);
                interruptionTo.setMax(null);
            }
            interruptionFrom.setValue(null);
            interruptionTo.setValue(null);
        });
        appBinderOperation.getBinder().forField(dateTo).asRequired(
                Transl.get(TextValues.CANNOT_BE_EMPTY)).bind(ApprovementDto::getDateTo, ApprovementDto::setDateTo);
        formLayout.add(dateTo);

        if (dto.getApprovementType() == ApprovementType.PAID_LEAVE) {
            ComboBox<EnumerationDto> enumType = enumerationComponentOperation.getEnumerationCombobox("PAID_LEAVE_TYPE");
            appBinderOperation.getBinder().forField(enumType).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(ApprovementDto::getEnumeration, ApprovementDto::setEnumeration);
            formLayout.add(enumType);
            if (dto.getEnumeration() != null) {
                enumType.setReadOnly(true);
            }
        }

        days.setReadOnly(true);
        formLayout.add(days);

        halfDay.addValueChangeListener(event -> {
            if (event.isFromClient()) {
                if (Boolean.TRUE.equals(event.getValue())) {
                    Double daysCount = dateFrom.getValue() == null
                            || holidayEntityList.contains(new HolidayEntity(dateFrom.getValue())) ? 0D : 0.5;
                    dateTo.setReadOnly(true);
                    if (dateFrom.getValue() != null) {
                        dateTo.setValue(dateFrom.getValue());
                    }
                    days.setValue(daysCount);
                    dto.setDays(daysCount);
                    dateFrom.setMax(null);
                } else {
                    dateTo.setReadOnly(false);
                    Double daysCount = dateFrom.getValue() == null
                            || holidayEntityList.contains(new HolidayEntity(dateFrom.getValue())) ? 0D : 1D;
                    days.setValue(daysCount);
                    dto.setDays(daysCount);
                }
            }
        });
        appBinderOperation.getBinder().forField(halfDay).bind(ApprovementDto::getHalfDay, ApprovementDto::setHalfDay);

        if (dto.getApprovementType() == ApprovementType.HOLIDAY) {
            formLayout.add(halfDay);
        }

        TextField created = new TextField(Transl.get("Created"));
        created.setValue(AppUtils.formatDateTime(dto.getCreated(), true));
        created.setReadOnly(true);

        TextField creator = new TextField(Transl.get("Creator"));
        creator.setValue(dto.getCreatedUserDto().getName());
        creator.setReadOnly(true);

        TextField state = new TextField(Transl.get("State"));
        state.setClassName(dto.getApprovementState().getColorClass());
        state.setValue(Transl.get(dto.getApprovementState().name()));
        state.setReadOnly(true);

        TextField lineManager = new TextField(Transl.get("Line manager"));
        if (dto.getApprovementType() != ApprovementType.PAID_LEAVE) {
            lineManager.setValue(dto.getLineManagerUserDto() == null ? "" : dto.getLineManagerUserDto().getName());
        }
        lineManager.setReadOnly(true);

        Checkbox approvedByLM = new Checkbox(
                Transl.get(dto.getApprovementType() == ApprovementType.PAID_LEAVE
                        ? "Approved by back office" : "Approved by line manager"));
        approvedByLM.setValue(dto.getLineManageApproved());
        approvedByLM.setReadOnly(true);

        TextField approvedByLMDate = new TextField(
                Transl.get(dto.getApprovementType() == ApprovementType.PAID_LEAVE
                        ? "Back office approved date" : "LM approved date"));
        approvedByLMDate.setValue(AppUtils.formatDateTime(dto.getLineManageApprovedDate(), true));
        approvedByLMDate.setReadOnly(true);

        TextField superior = new TextField(Transl.get("Superior"));
        superior.setValue(dto.getSuperiorUserDto().getName());
        superior.setReadOnly(true);

        Checkbox approvedSuperior = new Checkbox(Transl.get("Approved by superior"));
        approvedSuperior.setValue(dto.getSuperiorApproved());
        approvedSuperior.setReadOnly(true);

        TextField approvedSuperiorDate = new TextField(Transl.get("Superior approved date"));
        approvedSuperiorDate.setValue(AppUtils.formatDateTime(dto.getSuperiorApprovedDate(), true));
        approvedSuperiorDate.setReadOnly(true);

        if (dto.getId() != null) {
            if (dto.getApprovementType() == ApprovementType.ILL) {
                formLayout.add(created, state, lineManager, superior);
            } else if (dto.isLMSuperiorEquals()) {
                formLayout.add(created, state, superior, approvedSuperior, approvedSuperiorDate);
            } else {
                if (dto.getApprovementType() == ApprovementType.PAID_LEAVE) {
                    formLayout.add(created, creator, state, approvedByLM, approvedByLMDate, superior,
                            approvedSuperior, approvedSuperiorDate);
                } else {
                    formLayout.add(created, creator, state, lineManager, approvedByLM, approvedByLMDate, superior,
                            approvedSuperior, approvedSuperiorDate);
                }
            }
        }

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        this.add(formLayout);
        if (dto.getApprovementType() != ApprovementType.BUSSINES_TRIP
                && dto.getApprovementType() != ApprovementType.ILL) {
            this.add(getProjectsLayout(dto, !canEditBasicFields, appBinderOperation.getBinder(),
                    dto.getApprovementType().isEmployeeMandatory()));
        } else if (dto.getApprovementType() == ApprovementType.BUSSINES_TRIP) {
            this.add(getBusinessTripLayout(interruptionFrom, interruptionTo, dateFrom, dateTo));
        } else if (dto.getApprovementType() == ApprovementType.ILL) {
            this.add(getIllLayout());
        }

        HorizontalLayout noteLayout = new HorizontalLayout();
        noteLayout.setWidthFull();
        TextArea employeeNote = new TextArea(
                appBinderOperation.getDto().getApprovementType() != ApprovementType.ILL
                        ? Transl.get("Employee note") : Transl.get("Note"));
        employeeNote.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        appBinderOperation.getBinder().forField(employeeNote).bind(ApprovementDto::getNote, ApprovementDto::setNote);
        employeeNote.setWidthFull();
        employeeNote.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
        noteLayout.add(employeeNote);

        TextArea lMNote = new TextArea(Transl.get(dto.getApprovementType() == ApprovementType.PAID_LEAVE
                ? "Expression of back office" : "Expression of line manager"));
        lMNote.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        lMNote.setWidthFull();
        lMNote.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());

        if (lineManagerCanEdit && !master) {
            appBinderOperation.getBinder().forField(lMNote).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(ApprovementDto::getLineManageNote, ApprovementDto::setLineManageNote);
        } else {
            appBinderOperation.getBinder().forField(lMNote)
                    .bind(ApprovementDto::getLineManageNote, ApprovementDto::setLineManageNote);
            lMNote.setReadOnly(!master);
        }

        TextArea superiorNote = new TextArea(Transl.get("Expression of superior"));
        superiorNote.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        superiorNote.setWidthFull();
        superiorNote.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());

        if (superiorCanEdit && !master) {
            appBinderOperation.getBinder().forField(superiorNote).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(ApprovementDto::getSuperiorNote, ApprovementDto::setSuperiorNote);
        } else {
            appBinderOperation.getBinder().forField(superiorNote)
                    .bind(ApprovementDto::getSuperiorNote, ApprovementDto::setSuperiorNote);
            superiorNote.setReadOnly(!master);
        }

        if (dto.getId() != null && appBinderOperation.getDto().getApprovementType() != ApprovementType.ILL) {
            noteLayout.add(lMNote, superiorNote);
        }
        this.add(noteLayout);

        if (!canEditBasicFields) {
            dateFrom.setReadOnly(true);
            dateTo.setReadOnly(true);
            employeeNote.setReadOnly(true);
            halfDay.setReadOnly(true);
        }

    }

    public VerticalLayout getIllLayout() {
        VerticalLayout verticalLayout = new VerticalLayout();
        ComboBox<EmployeeDto> employee = getEmployeeCombobox(Transl.get("Employee"), !canEditBasicFields);
        appBinderOperation.getBinder().forField(employee).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ApprovementDto::getFirstEmployeeDto, ApprovementDto::setFirstEmployeeDto);
        verticalLayout.add(employee);
        return verticalLayout;
    }

    public VerticalLayout getBusinessTripLayout(DateTimePicker interruptionFrom, DateTimePicker interruptionTo, DatePicker dateFrom, DatePicker dateTo) {
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMargin(false);
        verticalLayout.setPadding(false);

        HorizontalLayout addressLayout = new HorizontalLayout();
        addressLayout.setAlignItems(Alignment.START);
        ComboBox<SubjectDto> subjectAddressFrom = new ComboBox<>(Transl.get("Address from"));
        subjectAddressFrom.setItems(subjectDtoList.stream().filter(subjectDto ->
                Boolean.FALSE.equals(subjectDto.getDeleted()) && Boolean.TRUE.equals(subjectDto.getOwnCompany())));
        subjectAddressFrom.setItemLabelGenerator(SubjectDto::getName);
        appBinderOperation.getBinder().forField(subjectAddressFrom)
                .bind(approvementDto -> approvementDto.getApprovementBusinessTripDto().getApprovementFromSubjectDto(),
                        (approvementDto, subjectDto) -> approvementDto.getApprovementBusinessTripDto()
                                .setApprovementFromSubjectDto(subjectDto));

        TextArea addressFrom = new TextArea();
        addressFrom.setReadOnly(true);
        subjectAddressFrom.addValueChangeListener(event -> {
            if (event.getValue() != null) {
                addressFrom.setValue(StringUtils.trimToEmpty(event.getValue().getAddress()));
            }
        });
        addressLayout.add(subjectAddressFrom);

        addressFrom.setClassName("address-text-area");
        addressLayout.add(addressFrom);
        if (subjectAddressFrom.getValue() != null) {
            addressFrom.setValue(subjectAddressFrom.getValue().getAddress());
        }

        ComboBox<SubjectDto> subjectAddressTo = new ComboBox<>(Transl.get("Address to"));
        List<SubjectDto> subjectAddressToList = subjectDtoList.stream().filter(subjectDto ->
                Boolean.FALSE.equals(subjectDto.getDeleted())
                        && !Boolean.TRUE.equals(subjectDto.getOwnCompany())).collect(Collectors.toList());
        SubjectDto otherSubjectDto = new SubjectDto();
        otherSubjectDto.setName(Transl.get("Own address"));
        subjectAddressToList.add(0, otherSubjectDto);

        subjectAddressTo.setItems(subjectAddressToList);
        subjectAddressTo.setItemLabelGenerator(SubjectDto::getName);
        appBinderOperation.getBinder().forField(subjectAddressTo)
                .bind(approvementDto -> approvementDto.getApprovementBusinessTripDto().getApprovementToSubjectDto(),
                        (approvementDto, subjectDto) ->
                                approvementDto.getApprovementBusinessTripDto().setApprovementToSubjectDto(subjectDto));
        addressLayout.add(subjectAddressTo);

        TextArea addressTo = new TextArea();
        subjectAddressTo.addValueChangeListener(event -> {
            if (event.getValue() != null && event.getValue().getId() != null) {
                addressTo.setValue(StringUtils.trimToEmpty(event.getValue().getAddress()));
                addressTo.setReadOnly(true);
            } else if (event.getValue().getId() == null) {
                addressTo.setValue("");
                addressTo.setReadOnly(false);
            }
        });
        addressTo.setClassName("address-text-area");
        addressTo.setReadOnly((!canEditBasicFields || subjectAddressTo.getValue() == null
                || subjectAddressTo.getValue().getId() != null) && !master);
        appBinderOperation.getBinder().forField(addressTo)
                .bind(approvementDto -> approvementDto.getApprovementBusinessTripDto().getApprovementToAnother(),
                        (approvementDto, address) ->
                                approvementDto.getApprovementBusinessTripDto().setApprovementToAnother(address));
        addressLayout.add(addressTo);

        if (subjectAddressTo.getValue() != null) {
            addressTo.setValue(subjectAddressTo.getValue().getAddress());
        }

        HorizontalLayout purposeLayout = new HorizontalLayout();
        ComboBox<EnumerationDto> purpose = enumerationComponentOperation.getEnumerationCombobox(
                Transl.get("Purpose"), "B_T_PURPOSE");
        purpose.setWidth("25em");
        appBinderOperation.getBinder().forField(purpose).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(approvementDto -> approvementDto.getApprovementBusinessTripDto().getPurposeDto(),
                        (approvementDto, businessTripPurpose) -> approvementDto.getApprovementBusinessTripDto()
                                .setPurposeDto(businessTripPurpose));
        purposeLayout.add(purpose);

        MultiSelectComboBox<BusinessTripTransportationType> transportation =
                new MultiSelectComboBox<>(Transl.get("Transportation"));
        transportation.setItems(BusinessTripTransportationType.values());
        transportation.setItemLabelGenerator(businessTripTransportationType ->
                Transl.get(businessTripTransportationType.name()));
        transportation.setWidth("25em");
        appBinderOperation.getBinder().forField(transportation).bind(approvementDto ->
                        approvementDto.getApprovementBusinessTripDto().getBusinessTripTransportationTypeSet(),
                (approvementDto, businessTripTransportationTypes) -> approvementDto.getApprovementBusinessTripDto()
                        .setBusinessTripTransportationTypeSet(businessTripTransportationTypes));
        purposeLayout.add(transportation);

        HorizontalLayout dateLayout = new HorizontalLayout();
        ComboBox<ProjectDto> project = new ComboBox<>(Transl.get("Project"));
        project.setItems(projectDtoList);
        project.setItemLabelGenerator(ProjectDto::getName);
        appBinderOperation.getBinder().forField(project).bind(approvementDto ->
                        approvementDto.getApprovementBusinessTripDto().getProjectDto(),
                (approvementDto, projectDto) ->
                        approvementDto.getApprovementBusinessTripDto().setProjectDto(projectDto));

        ComboBox<OpportunityDto> opportunity = new ComboBox<>(Transl.get("Opportunity"));
        opportunity.setItems(opportunityDtoList);
        opportunity.setItemLabelGenerator(OpportunityDto::getName);
        appBinderOperation.getBinder().forField(opportunity).bind(approvementDto ->
                        approvementDto.getApprovementBusinessTripDto().getOpportunityDto(),
                (approvementDto, opportunityDto) ->
                        approvementDto.getApprovementBusinessTripDto().setOpportunityDto(opportunityDto));
        dateLayout.add(project, opportunity);

        interruptionFrom.setWidth("25em");
        appBinderOperation.getBinder().forField(interruptionFrom)
                .withValidator((localDateTime, valueContext) -> {
                    if(localDateTime == null){
                        return ValidationResult.error(Transl.get(TextValues.CANNOT_BE_EMPTY));
                    } else if(interruptionTo.getValue() != null && interruptionTo.getValue().isBefore(localDateTime)){
                        return ValidationResult.error(Transl.get("date \"interruption to\" cannot be before date \"interruption from\""));
                    }
                    return ValidationResult.ok();
                })
                .bind(approvementDto -> approvementDto.getApprovementBusinessTripDto().getInterruptionFrom(),
                        (approvementDto, localDateTime) ->
                                approvementDto.getApprovementBusinessTripDto().setInterruptionFrom(localDateTime));
        dateLayout.add(interruptionFrom);

        interruptionTo.setWidth("25em");
        appBinderOperation.getBinder().forField(interruptionTo)
                .withValidator((localDateTime, valueContext) -> {
                    if(localDateTime == null){
                        return ValidationResult.error(Transl.get(TextValues.CANNOT_BE_EMPTY));
                    } else if(interruptionFrom.getValue() != null && interruptionFrom.getValue().isAfter(localDateTime)){
                        return ValidationResult.error(Transl.get("date \"interruption from\" cannot be after date \"interruption from\""));
                    }
                    return ValidationResult.ok();
                })
                .bind(approvementDto -> approvementDto.getApprovementBusinessTripDto().getInterruptionTo(),
                        (approvementDto, localDateTime) ->
                                approvementDto.getApprovementBusinessTripDto().setInterruptionTo(localDateTime));
        dateLayout.add(interruptionTo);


        MultiSelectComboBox<EmployeeDto> fellowPassengers = new MultiSelectComboBox<>(Transl.get("Fellow passengers"));
        fellowPassengers.setWidth("25em");
        fellowPassengers.setItems(employeeDtoList.stream()
                .filter(employeeDto -> !employeeDto.equals(userEmployeeDto)).toList());
        appBinderOperation.getBinder().forField(fellowPassengers).bind(approvementDto ->
                        approvementDto.getApprovementBusinessTripDto().getFellowPassengers(employeeDtoList),
                (approvementDto, employee) ->
                        approvementDto.getApprovementBusinessTripDto().setFellowPassengers(employee));
        fellowPassengers.setItemLabelGenerator(employeeDto ->
                employeeDto.getFirstName().concat(" ").concat(employeeDto.getLastName()));
        purposeLayout.add(fellowPassengers);

        verticalLayout.add(addressLayout, dateLayout, purposeLayout);

        if (!canEditBasicFields) {
            subjectAddressFrom.setReadOnly(true);
            subjectAddressTo.setReadOnly(true);
            addressTo.setReadOnly(true);
            project.setReadOnly(true);
            opportunity.setReadOnly(true);
            interruptionFrom.setReadOnly(true);
            interruptionTo.setReadOnly(true);
            purpose.setReadOnly(true);
            transportation.setReadOnly(true);
            fellowPassengers.setReadOnly(true);
        }
        return verticalLayout;
    }

    public HorizontalLayout getProjectsLayout(ApprovementDto approvementDto, boolean readOnly,
                                              Binder<ApprovementDto> binder, boolean showEmployee) {
        projectLayout = getHorizontalLayout();
        Button add = VaadinComponents.getButton(VaadinIcon.PLUS.create());
        add.addClickListener(buttonClickEvent -> {
            HorizontalLayout horizontalLayout = getHorizontalLayout();
            ComboBox<ProjectDto> project = getProjectComboBox(readOnly);
            horizontalLayout.add(project);
            if (showEmployee) {
                ComboBox<EmployeeDto> employee = getEmployeeCombobox(Transl.get("Representative"), readOnly);
                horizontalLayout.add(employee);
            }
            Button remove = new Button(VaadinIcon.MINUS.create());
            remove.addClickListener(event -> projectLayout.remove(horizontalLayout));
            horizontalLayout.add(remove);
            projectLayout.add(horizontalLayout);
        });
        if (!readOnly) {
            projectLayout.add(add);
        }
        if (approvementDto.getApprovementProjectEmployeeDtoList().isEmpty()) {
            HorizontalLayout horizontalLayout = getHorizontalLayout();
            ComboBox<ProjectDto> project = getProjectComboBox(false);
            binder.forField(project).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(ApprovementDto::getFirstProjectDto, ApprovementDto::setFirstProjectDto);
            horizontalLayout.add(project);
            if (showEmployee) {
                ComboBox<EmployeeDto> employee = getEmployeeCombobox(Transl.get("Representative"), false);
                binder.forField(employee).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(ApprovementDto::getFirstEmployeeDto, ApprovementDto::setFirstEmployeeDto);
                horizontalLayout.add(employee);
            }

            projectLayout.add(horizontalLayout);
        } else {
            addProjectsLayout(projectLayout, approvementDto.getApprovementProjectEmployeeDtoList(),
                    readOnly, binder, showEmployee);
        }

        return projectLayout;
    }

    private void addProjectsLayout(HorizontalLayout mainLayout, List<ApprovementProjectEmployeeDto> list,
                                   boolean readOnly, Binder<ApprovementDto> binder, boolean showEmployee) {
        for (int i = 0; i < list.size(); i++) {
            ApprovementProjectEmployeeDto appDto = list.get(i);
            HorizontalLayout horizontalLayout = getHorizontalLayout();
            ComboBox<ProjectDto> project = getProjectComboBox(readOnly);
            project.setValue(appDto.getProjectDto());
            horizontalLayout.add(project);
            if (showEmployee) {
                ComboBox<EmployeeDto> employee = getEmployeeCombobox(Transl.get("Representative"), readOnly);
                employee.setValue(appDto.getEmployeeDto());
                horizontalLayout.add(employee);
            }
            if (i != 0 && !readOnly) {
                Button remove = VaadinComponents.getButton(VaadinIcon.MINUS.create());
                remove.addClickListener(buttonClickEvent -> mainLayout.remove(horizontalLayout));
                horizontalLayout.add(remove);
            } else if (i == 0 && !readOnly) {
                binder.forField(project).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(ApprovementDto::getFirstProjectDto, ApprovementDto::setFirstProjectDto);
            }
            mainLayout.add(horizontalLayout);
        }
    }

    public ComboBox<ProjectDto> getProjectComboBox(boolean readOnly) {
        ComboBox<ProjectDto> project = new ComboBox<>(Transl.get("Project"));
        project.setItems(projectDtoList);
        project.setItemLabelGenerator(ProjectDto::getName);
        project.setReadOnly(readOnly);
        return project;
    }

    public ComboBox<EmployeeDto> getEmployeeCombobox(String title, boolean readOnly) {
        ComboBox<EmployeeDto> employee = new ComboBox<>(title);
        employee.setItems(employeeDtoList);
        employee.setItemLabelGenerator(employeeDto ->
                employeeDto.getFirstName().concat(" ").concat(employeeDto.getLastName()));
        employee.setClassName("approvement-employee-combobox");
        employee.setReadOnly(readOnly);
        return employee;
    }

    public HorizontalLayout getHorizontalLayout() {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.setPadding(false);
        horizontalLayout.setMargin(false);
        horizontalLayout.getElement().getStyle().set("align-items", "baseline");
        return horizontalLayout;
    }

    public HorizontalLayout getProjectLayout() {
        return projectLayout;
    }
}
