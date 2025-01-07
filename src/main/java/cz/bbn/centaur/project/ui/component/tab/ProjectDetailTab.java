package cz.bbn.cerberus.project.ui.component.tab;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.tab.TabDtoComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.ProjectComponentOperation;
import cz.bbn.cerberus.project.ProjectState;
import cz.bbn.cerberus.project.dto.ProjectColorEnum;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.project.ui.component.ProjectNewDialog;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Objects;

@Slf4j
public class ProjectDetailTab extends TabDtoComponent<ProjectDto> {

    private final List<ContractDto> contractList;
    private final List<UserDto> userList;
    private final boolean isDialog;
    private final boolean readOnly;
    private final ListService listService;
    private final ProjectDto dto;

    public ProjectDetailTab(ProjectDto dto, ProjectComponentOperation projectComponentOperation,
                            CustomPermissionSingleListener listener, ProjectNewDialog projectNewDialog,
                            AppEnv appEnv, boolean isDialog, boolean readOnly, ListService listService) {
        super(dto, projectComponentOperation.getSaveAction(listener, dto, projectNewDialog), appEnv);
        this.contractList = projectComponentOperation.getAllowedContractList();
        this.userList = projectComponentOperation.getUserList();
        this.isDialog = isDialog;
        this.readOnly = readOnly;
        this.listService = listService;
        this.dto = dto;
        initTab();
    }

    protected void initTab() {
        removeAll();
        this.setId(RobotFrameworkVariables.PROJECT_DETAIL_CARD_ID.getValue());
        this.setSizeFull();
        setMargin(false);
        setPadding(false);

        VerticalLayout verticalLayout = new VerticalLayout();

        FormLayout formLayout = new FormLayout();

        TextField id = new TextField(Transl.get("Id"));
        id.setMaxLength(20);
        id.setEnabled(getDto().getId() == null);
        id.setReadOnly(true);

        if (getDto().getId() != null) {
            getBinder().forField(id).bind(ProjectDto::getId, ProjectDto::setId);
            formLayout.add(id);
        }

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        getBinder().forField(name).asRequired(Transl.get(Transl.get(TextValues.CANNOT_BE_EMPTY)))
                .bind(ProjectDto::getName, ProjectDto::setName);
        formLayout.add(name);

        ComboBox<SubjectDto> customer = new ComboBox<>(Transl.get("Customer"));
        customer.setItems(listService.getSubjectDtoListByCustomer());
        getBinder().forField(customer).asRequired(Transl.get(Transl.get(TextValues.CANNOT_BE_EMPTY)))
                .bind(ProjectDto::getSubject, ProjectDto::setSubject);
        customer.setItemLabelGenerator(SubjectDto::getName);

        ComboBox<ContractDto> contract = new ComboBox<>(Transl.get("Contract"));
        contract.setItems(contractList);
        getBinder().forField(contract).bind(ProjectDto::getContract, ProjectDto::setContract);
        contract.setItemLabelGenerator(ContractDto::getName);

        customer.addValueChangeListener(event -> {
            if (event.isFromClient()) {
                contract.setValue(null);
                contract.setItems(contractList.stream()
                        .filter(contractDto -> contractDto.getSubjectDto().getId().equals(event.getValue().getId())));
            }
        });

        contract.addValueChangeListener(event -> {
            if (event.getValue() != null && event.getValue().getSubjectDto() != null && event.isFromClient()) {
                customer.setValue(event.getValue().getSubjectDto());
            }
        });

        formLayout.add(customer, contract);

        if (getDto().getId() != null && !SecurityUtils.hasPermission(Permission.PROJECT_CHANGE_CUS_CONT)) {
            customer.setReadOnly(true);
            contract.setReadOnly(true);
        }

        ComboBox<ProjectColorEnum> colorComboBox = new ComboBox<>(Transl.get("Calendar color"));
        colorComboBox.setItems(ProjectColorEnum.values());
        colorComboBox.setItemLabelGenerator(ProjectColorEnum::getName);
        getBinder().forField(colorComboBox).asRequired(Transl.get(Transl.get(TextValues.CANNOT_BE_EMPTY)))
                .bind(ProjectDto::getColorEnum, ProjectDto::setColorEnum);
        formLayout.add(colorComboBox);

        DatePicker startTime = VaadinComponents.getDatePicker(getDto().getStartTime());
        startTime.setLabel(Transl.get("Start time"));
        getBinder().forField(startTime).bind(ProjectDto::getStartTime, ProjectDto::setStartTime);
        formLayout.add(startTime);

        DatePicker endTime = VaadinComponents.getDatePicker(getDto().getEndTime());
        endTime.setLabel(Transl.get("End time"));
        getBinder().forField(endTime).bind(ProjectDto::getEndTime, ProjectDto::setEndTime);
        formLayout.add(endTime);

        startTime.setMax(endTime.getValue());
        endTime.setMin(startTime.getValue());
        startTime.addValueChangeListener(e -> endTime.setMin(e.getValue()));
        endTime.addValueChangeListener(e -> startTime.setMax(e.getValue()));

        ComboBox<UserDto> user = new ComboBox<>(Transl.get("Owner"));
        user.setItems(userList);
        user.setItemLabelGenerator(UserDto::getName);
        getBinder().forField(user).asRequired(Transl.get(Transl.get(TextValues.CANNOT_BE_EMPTY)))
                .bind(ProjectDto::getUserDto, ProjectDto::setUserDto);

        if (getBinder().getBean().getId() == null) {
            user.setValue(SecurityUtils.appUserToUserDto());
        }

        if (getBinder().getBean().getId() != null && !SecurityUtils.hasPermission(Permission.CHANGE_OWNER)
                && !Objects.equals(getBinder().getBean().getUserDto().getId(), SecurityUtils.getCurrentUserId())) {
            user.setReadOnly(true);
        }

        formLayout.add(user);

        ComboBox<ProjectState> projectState = new ComboBox<>(Transl.get("State"));
        projectState.setItems(ProjectState.values());
        projectState.setItemLabelGenerator(actualProjectState -> Transl.get(actualProjectState.name()));
        projectState.setReadOnly(getDto().getId() == null || (!SecurityUtils.getAllowedEntityIdByDomain(
                Permission.PROJECT_EDIT.name(), DomainEnum.PROJECT_DOMAIN_NAME.getValue()).contains(getDto().getId())));
        getBinder().forField(projectState)
                .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ProjectDto::getProjectState, ProjectDto::setProjectState);
        formLayout.add(projectState);

        verticalLayout.add(formLayout);
        verticalLayout.setHeightFull();

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(255);
        getBinder().forField(description)
                .bind(ProjectDto::getDescription, ProjectDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());
        verticalLayout.add(description);

        getBinder().setBean(getDto());

        if (isDialog) {
            verticalLayout.setPadding(false);
            verticalLayout.setMargin(false);
        }

        if (readOnly) {
            name.setReadOnly(true);
            contract.setReadOnly(true);
            customer.setReadOnly(true);
            colorComboBox.setReadOnly(true);
            startTime.setReadOnly(true);
            endTime.setReadOnly(true);
            user.setReadOnly(true);
            description.setReadOnly(true);
            projectState.setReadOnly(true);
        }

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        this.add(verticalLayout);
    }

}
