package cz.bbn.cerberus.project.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.activity.ActivityByObjectComponentOperation;
import cz.bbn.cerberus.activity.dto.ActivityLinkDto;
import cz.bbn.cerberus.activity.ui.component.ActivityLinkDialog;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.ContactPersonComponentOperation;
import cz.bbn.cerberus.contactperson.ContactPersonService;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonByObjectFilterDto;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contactperson.dto.TypeByObject;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonFilterComponent;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonLinkDialog;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeService;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.custompermission.ui.ChangeAffectedUsersAction;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.custompermission.ui.component.CustomPermissionSinglePermissionTabComponent;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.employee.dto.EmployeeLinkDto;
import cz.bbn.cerberus.employee.ui.component.EmployeeLinkDialog;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.ui.component.NoteComponent;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.phase.PhaseComponentOperation;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.phase.ui.component.PhaseNewDialog;
import cz.bbn.cerberus.project.ProjectComponentOperation;
import cz.bbn.cerberus.project.ProjectService;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.project.ui.component.ProjectTabsComponent;
import cz.bbn.cerberus.project.ui.component.tab.ProjectActivityTab;
import cz.bbn.cerberus.project.ui.component.tab.ProjectContactPersonTab;
import cz.bbn.cerberus.project.ui.component.tab.ProjectDetailTab;
import cz.bbn.cerberus.project.ui.component.tab.ProjectDocumentTab;
import cz.bbn.cerberus.project.ui.component.tab.ProjectEmailTab;
import cz.bbn.cerberus.project.ui.component.tab.ProjectEmployeeTab;
import cz.bbn.cerberus.project.ui.component.tab.ProjectPhaseTab;
import cz.bbn.cerberus.project.ui.component.tab.ProjectSupplierTab;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.ui.component.SubjectLinkDialog;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.ui.component.TaskSlideTabComponent;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

@Route(value = ProjectDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.PROJECT_VIEW)
@Slf4j
public class ProjectDetailView
        extends AppView implements HasUrlParameter<String>, CustomPermissionSingleListener {

    public static final String ROUTE = "project-detail";

    private final AppEnv appEnv;
    private final ProjectService projectService;
    private final NoteComponentOperation noteComponentOperation;
    private final ContactPersonService contactPersonService;
    private final ListService listService;
    private final UserService userService;
    private final ProjectComponentOperation projectComponentOperation;
    private final ContactPersonTypeService contactPersonTypeService;
    private final ContactPersonComponentOperation contactPersonComponentOperation;
    private final SubjectComponentOperation subjectComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final EmailComponentOperations emailComponentOperations;

    private ProjectContactPersonTab projectContactPersonTab;
    private ProjectDto dto;

    private Set<PermUserDto> userSet;
    private boolean readOnly = true;

    private final DocumentComponentOperation documentComponentOperation;
    private final TaskComponentOperation taskComponentOperation;
    private final ActivityByObjectComponentOperation activityByObjectComponentOperation;
    private final PhaseComponentOperation phaseComponentOperation;
    private final EmployeeComponentOperation employeeComponentOperation;

    public ProjectDetailView(AppEnv appEnv, ProjectService projectService,
                             NoteComponentOperation noteComponentOperation, ContactPersonService contactPersonService,
                             DocumentComponentOperation documentComponentOperation,
                             ListService listService, UserService userService,
                             ProjectComponentOperation projectComponentOperation,
                             ContactPersonTypeService contactPersonTypeService,
                             ContactPersonComponentOperation contactPersonComponentOperation,
                             SubjectComponentOperation subjectComponentOperation,
                             EntityNewComponentOperation entityNewComponentOperation,
                             EmailComponentOperations emailComponentOperations, TaskComponentOperation taskComponentOperation,
                             ActivityByObjectComponentOperation activityByObjectComponentOperation,
                             PhaseComponentOperation phaseComponentOperation,
                             EmployeeComponentOperation employeeComponentOperation) {
        this.appEnv = appEnv;
        this.projectService = projectService;
        this.noteComponentOperation = noteComponentOperation;
        this.contactPersonService = contactPersonService;
        this.documentComponentOperation = documentComponentOperation;
        this.projectComponentOperation = projectComponentOperation;
        this.listService = listService;
        this.userService = userService;
        this.contactPersonTypeService = contactPersonTypeService;
        this.contactPersonComponentOperation = contactPersonComponentOperation;
        this.subjectComponentOperation = subjectComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.emailComponentOperations = emailComponentOperations;
        this.taskComponentOperation = taskComponentOperation;
        this.activityByObjectComponentOperation = activityByObjectComponentOperation;
        this.phaseComponentOperation = phaseComponentOperation;
        this.employeeComponentOperation = employeeComponentOperation;
    }

    private void initView() {
        setSizeFull();
        removeAll();
        List<TabEntry> tabEntryList = new ArrayList<>();
        tabEntryList.add(new TabEntry(Transl.get("Project detail"),
                new ProjectDetailTab(dto, projectComponentOperation, this, null,
                        appEnv, false, readOnly, listService)));

        String heading = dto.getId() == null ? Transl.get("New project") :
                Transl.get("Project")
                        .concat(" - ")
                        .concat(dto.getName());

        projectContactPersonTab =
                new ProjectContactPersonTab(
                        getContactPersonRemoveAction(), getContactPersonItemsAction(), appEnv
                );
        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_VIEW)) {
            tabEntryList.add(new TabEntry(
                    Transl.get("Contact person"), projectContactPersonTab, Permission.CONTACT_PERSON_VIEW));
        }

        ProjectSupplierTab projectSupplierTab = new ProjectSupplierTab(
                subjectComponentOperation, dto.getId(), appEnv, listService.getSupplierTypeMap());

        if (SecurityUtils.hasPermission(Permission.SUPPLIER_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Supplier"), projectSupplierTab, Permission.SUPPLIER_VIEW));
        }

        if (Objects.equals(SecurityUtils.getCurrentUserId(), dto.getUserDto().getId())) {
            tabEntryList.add(new TabEntry(Transl.get("Permission"), new CustomPermissionSinglePermissionTabComponent(
                    getChangeAffectedUsersAction(), Permission.PROJECT_VIEW.name(),
                    DomainEnum.PROJECT_DOMAIN_NAME.getValue(), userService.findUserList(),
                    dto.getId(), Transl.get("Add read permission to object"))
            ));
        }

        Button addDocument = VaadinComponents.getNewButton(Transl.get("Add new document"), false);
        if (SecurityUtils.hasPermission(Permission.DOCUMENT_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Documents"),
                    new ProjectDocumentTab(
                            documentComponentOperation, dto, !readOnly, appEnv, addDocument, listService),
                    Permission.PROJECT_DOCUMENT_VIEW));
        }

        if (SecurityUtils.hasCustomPermission(DomainEnum.PROJECT_DOMAIN_NAME.getValue(),
                dto.getId(), Permission.PROJECT_EMAIL_VIEW.name())) {
            ProjectEmailTab projectEmailTab = new ProjectEmailTab(dto, emailComponentOperations, appEnv);
            tabEntryList.add(new TabEntry(Transl.get("Email list"),
                    projectEmailTab));
        }

        Button linkActivity = VaadinComponents.getLinkButton(Transl.get("Link activity"));
        ProjectActivityTab projectActivityTab =
                new ProjectActivityTab(dto.getId(), appEnv, activityByObjectComponentOperation);
        tabEntryList.add(new TabEntry(Transl.get("Activity"),
                projectActivityTab,
                Permission.ACTIVITY_BY_OBJECT_VIEW));
        linkActivity.addClickListener(buttonClickEvent -> {
            ActivityLinkDto activityLinkDto = new ActivityLinkDto();
            activityLinkDto.setObjectId(dto.getId());
            activityLinkDto.setObjectType(ObjectType.PROJECT);
            activityLinkDto.setActivityDtoSet(new HashSet<>());
            ActivityLinkDialog activityLinkDialog = new ActivityLinkDialog(listService,
                    activityByObjectComponentOperation.getSaveAction(),
                    projectActivityTab.getGrid(), activityLinkDto, appEnv,
                    activityByObjectComponentOperation.getLinkedActivityEmployeeList(dto.getId(), ObjectType.PROJECT));
            activityLinkDialog.open();
        });

        Button linkEmployee = VaadinComponents.getLinkButton(Transl.get("Link team member"));
        ProjectEmployeeTab employeeTab = new ProjectEmployeeTab(dto.getId(), appEnv, employeeComponentOperation);
        tabEntryList.add(new TabEntry(Transl.get("Team member"),
                employeeTab,
                Permission.EMPLOYEE_BY_OBJECT_VIEW));
        linkEmployee.addClickListener(buttonClickEvent -> {
            EmployeeLinkDto employeeLinkDto = new EmployeeLinkDto();
            employeeLinkDto.setObjectType(ObjectType.PROJECT);
            employeeLinkDto.setObjectId(dto.getId());
            employeeLinkDto.setEmployeeDtoSet(new HashSet<>());
            EmployeeLinkDialog employeeLinkDialog = new EmployeeLinkDialog(
                    employeeComponentOperation.getEmployeeByObjectDtoSaveAction(),
                    employeeTab.getGrid(), listService, employeeLinkDto, appEnv,
                    employeeComponentOperation.getLinkedEmployeeList(dto.getId(), ObjectType.PROJECT));
            employeeLinkDialog.open();
        });

        Button addPhase = VaadinComponents.getNewButton(Transl.get("Add phase"), false);
        boolean phaseReadOnly = !SecurityUtils.hasPermission(Permission.PHASE_EDIT) && readOnly;
        ProjectPhaseTab phaseTab =
                new ProjectPhaseTab(dto.getId(), appEnv, phaseComponentOperation, phaseReadOnly);
        tabEntryList.add(new TabEntry(Transl.get("Phase"),
                phaseTab,
                Permission.PHASE_VIEW));
        addPhase.addClickListener(buttonClickEvent -> {
            PhaseDto phaseDto = new PhaseDto();
            phaseDto.setProjectId(dto.getId());
            PhaseNewDialog phaseNewDialog = new PhaseNewDialog(
                    phaseDto, phaseComponentOperation, phaseTab.getGrid(), appEnv, phaseReadOnly);
            phaseNewDialog.open();
        });

        Button linkContactPerson = VaadinComponents.getLinkButton(Transl.get("Link contact person"));
        linkContactPerson.addClickListener(e -> {
            Button contactSearch = VaadinComponents.getSearchButton();
            ContactPersonFilterComponent filter = new ContactPersonFilterComponent(
                    contactPersonTypeService.findAllEnabled(), contactSearch);
            String subjectId = dto.getSubject() != null ? dto.getSubject().getId() : "";
            ContactPersonLinkDialog linkDialog = new ContactPersonLinkDialog(
                    contactPersonComponentOperation.getObjectSubjectLinkItemAction(
                            filter, subjectId, dto.getId(), ContactPersonObjectTypeEnum.PROJECT),
                    contactPersonComponentOperation.getLinkContactEvent(
                            projectContactPersonTab.getGrid(), dto.getId(), ContactPersonObjectTypeEnum.PROJECT),
                    filter, appEnv);
            contactSearch.addClickListener(event -> linkDialog.loadData());
            linkDialog.open();
        });

        Button linkSupplier = VaadinComponents.getLinkButton(Transl.get("Link supplier"));
        linkSupplier.addClickListener(buttonClickEvent -> {
            SubjectLinkDialog dialog =
                    new SubjectLinkDialog(Transl.get("Link supplier"), subjectComponentOperation,
                            appEnv, dto.getId(), ObjectType.PROJECT, projectSupplierTab.getGrid());
            dialog.open();
        });

        TaskSlideTabComponent taskSlideTabComponent = new TaskSlideTabComponent(
                taskComponentOperation, appEnv, ObjectType.PROJECT,
                dto.getId(), dto.getSubject(), listService);

        ProjectTabsComponent tabsComponent =
                new ProjectTabsComponent(heading, tabEntryList, linkContactPerson,
                        linkSupplier, !readOnly, addDocument, linkActivity, linkEmployee,
                        addPhase, entityNewComponentOperation, dto);

        tabsComponent.showSubjectLink(dto.getSubject());
        tabsComponent.addEventSlideTab(taskSlideTabComponent);

        List<TypeByObject> typeByObjectList = new ArrayList<>();
        typeByObjectList.add(new TypeByObject(dto.getId(), ContactPersonObjectTypeEnum.PROJECT));
        typeByObjectList.add(new TypeByObject(dto.getSubject().getId(), ContactPersonObjectTypeEnum.SUBJECT));
        tabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent
                .Builder(entityNewComponentOperation, EntityNewType.SALES)
                .setSubjectDto(dto.getSubject())
                .setOpportunityFillSubjectDto(false)
                .setContractFillSubjectDto(false)
                .setOfferFillSubjectDto(false)
                .setTypeByObjectList(typeByObjectList)
                .build());

        if (dto.getId() != null && SecurityUtils.hasPermission(Permission.PROJECT_NOTE_VIEW)) {
            Checkbox showArchived = new Checkbox(Transl.get("Show also archived"));
            boolean canViewCustomPermission = SecurityUtils.hasCustomPermission(
                    DomainEnum.PROJECT_DOMAIN_NAME.getValue(), dto.getId(), Permission.PROJECT_NOTE_VIEW.name());
            boolean canEditCustomPermission = SecurityUtils.hasCustomPermission(
                    DomainEnum.PROJECT_DOMAIN_NAME.getValue(), dto.getId(), Permission.PROJECT_NOTE_EDIT.name());
            CountIntIndicator noteIndicator = new CountIntIndicator(
                    noteComponentOperation.getNoteCountByTypeAndObjectId(NoteTypeEnum.PROJECT, dto.getId()));
            NoteComponent noteComponent = new NoteComponent(noteComponentOperation, showArchived,
                    dto.getId(), appEnv,
                    NoteTypeEnum.PROJECT,
                    Permission.PROJECT_NOTE_VIEW, Permission.PROJECT_NOTE_EDIT,
                    canViewCustomPermission, canEditCustomPermission, noteIndicator);
            tabsComponent.addNoteSlideTab(noteComponent, noteIndicator);
        }

        add(tabsComponent);
    }


    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                if (SecurityUtils.hasCustomPermission(
                        DomainEnum.PROJECT_DOMAIN_NAME.getValue(), param, Permission.PROJECT_VIEW.name())) {
                    dto = projectService.getProject(param);
                    refreshBreadcrumbText(dto.getName());
                    if (SecurityUtils.hasCustomPermission(
                            DomainEnum.PROJECT_DOMAIN_NAME.getValue(), param, Permission.PROJECT_EDIT.name())) {
                        readOnly = false;
                    }
                    initView();
                } else {
                    ErrorNotification.show(ErrorCode.VIEW_PERMISSION_MISSING.getError(), appEnv);
                    UI.getCurrent().access(
                            () -> UI.getCurrent().getPage().fetchCurrentURL(e -> UI.getCurrent().navigate(e.getPath()))
                    );
                }
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }

    private ChangeAffectedUsersAction getChangeAffectedUsersAction() {
        return userTempSet -> this.userSet = userTempSet;
    }

    private DeleteAction getContactPersonRemoveAction() {
        return id -> {
            contactPersonService.deleteAddedContactPerson(id, ContactPersonObjectTypeEnum.PROJECT, dto.getId());
            SuccessNotification.showDeleteSuccess(appEnv);
            projectContactPersonTab.loadData();
        };
    }

    private ItemsAction<ContactPersonByObjectDto> getContactPersonItemsAction() {
        return (query, orderList) -> {
            ContactPersonByObjectFilterDto filterDto = new ContactPersonByObjectFilterDto();
            filterDto.setPage(query.getPage());
            filterDto.setSize(query.getPageSize());
            filterDto.setOrderList(orderList);
            filterDto.setObjectType(ContactPersonObjectTypeEnum.PROJECT);
            filterDto.setObjectId(dto.getId());
            return contactPersonService.findSubjectContactPageByObjectPage(filterDto);
        };
    }

    @Override
    public Set<PermUserDto> getSinglePermissionUserList() {
        return userSet;
    }
}
