package cz.bbn.cerberus.approvement.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.approvement.ApprovementComponentOperation;
import cz.bbn.cerberus.approvement.dto.ApprovementDto;
import cz.bbn.cerberus.approvement.dto.ApprovementSimpleDto;
import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.AppBinderOperation;
import cz.bbn.cerberus.commons.component.ui.interfaces.CountActionDouble;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.role.dto.RoleDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.SerializationUtils;

import java.time.LocalDateTime;
import java.util.List;

public class ApprovementDialog extends AppDialog implements AppBinderOperation<ApprovementDto> {

    private final ApprovementType type;
    private final AppInfiniteGrid<ApprovementSimpleDto> grid;
    private final ApprovementComponentOperation approvementComponentOperation;
    private final ApprovementDto dto;
    private final ApprovementDto originalDto;
    private final List<ProjectDto> projectDtoList;
    private final List<EmployeeDto> employeeDtoList;
    private final List<HolidayEntity> holidayEntityList;
    private final CountActionDouble countActionDouble;
    private final List<RoleDto> roleList;
    private final List<SubjectDto> subjectDtoList;
    private final List<OpportunityDto> opportunityDtoList;
    private final EmployeeDto userEmployeeDto;
    private final EnumerationComponentOperation enumerationComponentOperation;
    private final Binder<ApprovementDto> binder = new Binder<>();

    private final boolean master;

    public ApprovementDialog(ApprovementType type, AppInfiniteGrid<ApprovementSimpleDto> grid,
                             ApprovementComponentOperation approvementComponentOperation,
                             ApprovementDto dto, List<ProjectDto> projectDtoList, List<EmployeeDto> employeeDtoList,
                             List<HolidayEntity> holidayEntityList, CountActionDouble countActionDouble,
                             List<RoleDto> roleList, List<SubjectDto> subjectDtoList,
                             List<OpportunityDto> opportunityDtoList, EmployeeDto userEmployeeDto,
                             EnumerationComponentOperation enumerationComponentOperation, boolean master) {
        this.type = type;
        this.grid = grid;
        this.approvementComponentOperation = approvementComponentOperation;
        this.dto = dto;
        this.projectDtoList = projectDtoList;
        this.originalDto = SerializationUtils.clone(dto);
        this.employeeDtoList = employeeDtoList;
        this.holidayEntityList = holidayEntityList;
        this.countActionDouble = countActionDouble;
        this.roleList = roleList;
        this.subjectDtoList = subjectDtoList;
        this.opportunityDtoList = opportunityDtoList;
        this.userEmployeeDto = userEmployeeDto;
        this.enumerationComponentOperation = enumerationComponentOperation;
        this.master = master;
        initComponent();
    }

    private void initComponent() {
        String title = dto.getId() == null ?
                Transl.get("New".concat(" ").concat(type.name())) :
                Transl.get(type.name().toLowerCase()).concat(" ")
                        .concat(Transl.get("detail - {0}", String.valueOf(dto.getId())));
        setTitle(title);

        ApprovementDetailComponent approvementDetailComponent =
                new ApprovementDetailComponent(this, creatorCanEdit() || master, lineManagerCanEdit(),
                        superiorCanEdit(), projectDtoList, employeeDtoList, holidayEntityList, subjectDtoList,
                        opportunityDtoList, enumerationComponentOperation, userEmployeeDto, master);
        setContent(approvementDetailComponent);

        addCloseButton();

        if (creatorCanEdit() || master) {
            Button submit = master ? VaadinComponents.getButton(Transl.get("Submit"), VaadinIcon.CLOUD_UPLOAD.create())
                    : VaadinComponents.getSubmitButton();
            submit.addClickListener(event -> {
                if (binder.validate().isOk()) {
                    if (dto.getApprovementState() == ApprovementState.RETURNED_FOR_COMPLETION) {
                        if (dto.isLMSuperiorEquals() || Boolean.TRUE.equals(dto.getLineManageApproved())) {
                            dto.setApprovementState(ApprovementState.WAITING_FOR_SUPERIOR);
                        } else {
                            dto.setApprovementState(ApprovementState.WAIT_FOR_LM_MANAGER);
                        }
                    }
                    approvementComponentOperation.getSaveAction(
                            this,  master, binder, approvementDetailComponent, grid,
                            countActionDouble).saveItem(dto, originalDto);
                    grid.loadData();
                    if (countActionDouble != null) {
                        countActionDouble.getCount();
                    }
                    submit.setEnabled(true);
                }
            });
            addButtons(submit);
        }
        if (dto.getId() != null && (lineManagerCanEdit() || superiorCanEdit() || master)) {
            Button returnForCompletion = VaadinComponents.getButton(
                    Transl.get("Return for completion"), VaadinIcon.BACKWARDS.create());
            returnForCompletion.addClickListener(event -> {
                if (binder.validate().isOk()) {
                    dto.setApprovementState(ApprovementState.RETURNED_FOR_COMPLETION);
                    approvementComponentOperation.getSaveAction(
                            this, master, binder, approvementDetailComponent, grid, countActionDouble).saveItem(dto, originalDto);
                    grid.loadData();
                }
            });
            addButtons(returnForCompletion);

            Button reject = VaadinComponents.getButton(Transl.get("Reject"), VaadinIcon.BAN.create());
            reject.addClickListener(event -> {
                if (binder.validate().isOk()) {
                    dto.setApprovementState(ApprovementState.DENIED);
                    approvementComponentOperation.getSaveAction(
                            this, master,
                            binder, approvementDetailComponent, grid, countActionDouble).saveItem(dto, originalDto);
                    grid.loadData();
                    if (countActionDouble != null) {
                        countActionDouble.getCount();
                    }
                }
            });
            addButtons(reject);

            Button approve = VaadinComponents.getButton(Transl.get("Approve"), VaadinIcon.CHECK.create());
            approve.getElement().setProperty(TextValues.TITLE, Transl.get("Approve"));
            approve.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
            approve.addClickListener(event -> {
                if (binder.validate().isOk()) {
                    if (lineManagerCanEdit() && !master) {
                        dto.setApprovementState(ApprovementState.WAITING_FOR_SUPERIOR);
                        dto.setLineManageApprovedDate(LocalDateTime.now());
                        dto.setLineManageApproved(true);
                    } else {
                        dto.setApprovementState(ApprovementState.COMPLETELY_APPROVED);
                        dto.setSuperiorApprovedDate(LocalDateTime.now());
                        dto.setSuperiorApproved(true);
                    }
                    if (binder.validate().isOk()) {
                        approvementComponentOperation.getSaveAction(
                                this, master, binder, approvementDetailComponent, grid, countActionDouble).saveItem(dto, originalDto);
                        grid.loadData();
                    }
                }
            });
            addButtons(approve);
        } else if (dto.getId() != null && (dto.getApprovementType() == ApprovementType.ILL &&
                (approvementComponentOperation.isBackOffice(getDto().getApprovementType(), roleList) || master))) {
            Button reject = VaadinComponents.getButton(Transl.get("Cancel"), VaadinIcon.BAN.create());
            reject.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
            reject.addClickListener(event -> {
                if (binder.validate().isOk()) {
                    dto.setApprovementState(ApprovementState.CANCELED);
                    approvementComponentOperation.getSaveAction(
                            this, master, binder, approvementDetailComponent, grid, countActionDouble).saveItem(dto, originalDto);
                    grid.loadData();
                    if (countActionDouble != null) {
                        countActionDouble.getCount();
                    }
                }
            });
            addButtons(reject);
        }

        //@TODO - pokusit se tuhle funkcionalitu aplikovat globalne pro vsechny dialogy. Velikost by se mohla prirazovat po zavolani funkce setContent v AppDialogu
        setHeight(approvementDetailComponent.getHeight() + 15);
        setWidth("85%");
    }

    @Override
    public Binder<ApprovementDto> getBinder() {
        binder.setBean(getDto());
        return binder;
    }

    @Override
    public ApprovementDto getDto() {
        return dto;
    }

    private boolean creatorCanEdit() {
        return dto.getId() == null
                || (getDto().getApprovementState() == ApprovementState.RETURNED_FOR_COMPLETION
                && dto.getOwnerUserDto().getId().equals(SecurityUtils.getCurrentUserDto().getId()));
    }


    private boolean lineManagerCanEdit() {
        boolean hasBackOffice = approvementComponentOperation.isBackOffice(getDto().getApprovementType(), roleList);
        return getDto().getId() != null
                && getDto().getApprovementState() == ApprovementState.WAIT_FOR_LM_MANAGER
                && !getDto().isLMSuperiorEquals()
                && (getDto().getLineManagerUserDto() != null
                && getDto().getLineManagerUserDto().getId().equals(SecurityUtils.getCurrentUserDto().getId())
                || (getDto().getLineManagerRole() != null && hasBackOffice));
    }

    private boolean superiorCanEdit() {
        return getDto().getId() != null
                && getDto().getSuperiorUserDto().getId().equals(SecurityUtils.getCurrentUserDto().getId())
                && ((getDto().getApprovementState() == ApprovementState.WAITING_FOR_SUPERIOR)
                || (getDto().getApprovementState() == ApprovementState.WAIT_FOR_LM_MANAGER
                && getDto().getLineManagerUserDto() == null)
                || (getDto().getApprovementState() == ApprovementState.WAIT_FOR_LM_MANAGER
                && getDto().isLMSuperiorEquals()));
    }
}
