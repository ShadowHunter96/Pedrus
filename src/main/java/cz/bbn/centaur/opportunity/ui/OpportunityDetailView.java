package cz.bbn.cerberus.opportunity.ui;

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
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
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
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.ContactPersonComponentOperation;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contactperson.dto.TypeByObject;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonFilterComponent;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonLinkDialog;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeService;
import cz.bbn.cerberus.contract.ContractComponentOperation;
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
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.opportunity.OpportunityComponentOperation;
import cz.bbn.cerberus.opportunity.OpportunityService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.ui.component.OpportunityTabsComponent;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunityActivityTab;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunityContactPersonTab;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunityDetailTab;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunityDocumentTab;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunityEmailTab;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunityEmployeeTab;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunityOfferTab;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunitySubSupplierListTab;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.ui.component.TaskSlideTabComponent;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

@Route(value = OpportunityDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.OPPORTUNITY_VIEW)
@Slf4j
public class OpportunityDetailView extends AppView implements HasUrlParameter<String>, CustomPermissionSingleListener {

    public static final String ROUTE = "opportunity-detail";

    private OpportunityDto dto;

    private final AppEnv appEnv;
    private final OpportunityService opportunityService;
    private final OpportunityComponentOperation opportunityComponentOperation;
    private final NoteComponentOperation noteComponentOperation;
    private final DocumentComponentOperation documentComponentOperation;
    private final UserService userService;
    private final ContactPersonTypeService contactPersonTypeService;
    private final ContactPersonComponentOperation contactPersonComponentOperation;
    private final OfferComponentOpperation offerComponentOpperation;
    private final ListService listService;
    private final SubjectComponentOperation subjectComponentOperation;
    private final TaskComponentOperation taskComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final EmployeeComponentOperation employeeComponentOperation;
    private final ActivityByObjectComponentOperation activityByObjectComponentOperation;
    private final AreaTechnologyComponentOperation areaTechnologyComponentOperation;
    private final EmailComponentOperations emailComponentOperations;
    private final SubjectService subjectService;
    private final ContractComponentOperation contractComponentOperation;

    private Set<PermUserDto> userSet = new HashSet<>();
    private boolean readOnly = true;

    public OpportunityDetailView(AppEnv appEnv, OpportunityService opportunityService,
                                 OpportunityComponentOperation opportunityComponentOperation,
                                 NoteComponentOperation noteComponentOperation,
                                 DocumentComponentOperation documentComponentOperation,
                                 UserService userService, ContactPersonTypeService contactPersonTypeService,
                                 ContactPersonComponentOperation contactPersonComponentOperation,
                                 OfferComponentOpperation offerComponentOpperation, ListService listService,
                                 SubjectComponentOperation subjectComponentOperation,
                                 TaskComponentOperation taskComponentOperation,
                                 EntityNewComponentOperation entityNewComponentOperation,
                                 EmployeeComponentOperation employeeComponentOperation,
                                 ActivityByObjectComponentOperation activityByObjectComponentOperation,
                                 AreaTechnologyComponentOperation areaTechnologyComponentOperation,
                                 EmailComponentOperations emailComponentOperations, SubjectService subjectService,
                                 ContractComponentOperation contractComponentOperation) {
        this.appEnv = appEnv;
        this.opportunityService = opportunityService;
        this.opportunityComponentOperation = opportunityComponentOperation;
        this.noteComponentOperation = noteComponentOperation;
        this.documentComponentOperation = documentComponentOperation;
        this.userService = userService;
        this.contactPersonTypeService = contactPersonTypeService;
        this.contactPersonComponentOperation = contactPersonComponentOperation;
        this.offerComponentOpperation = offerComponentOpperation;
        this.listService = listService;
        this.subjectComponentOperation = subjectComponentOperation;
        this.taskComponentOperation = taskComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.employeeComponentOperation = employeeComponentOperation;
        this.activityByObjectComponentOperation = activityByObjectComponentOperation;
        this.areaTechnologyComponentOperation = areaTechnologyComponentOperation;
        this.emailComponentOperations = emailComponentOperations;
        this.subjectService = subjectService;
        this.contractComponentOperation = contractComponentOperation;
    }

    private void initView() {
        setSizeFull();
        removeAll();

        AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent = new AreaTechnologySignsBadgeComponent(
                areaTechnologyComponentOperation, ObjectType.OPPORTUNITY, dto.getId());

        List<TabEntry> tabEntryList = new ArrayList<>();
        tabEntryList.add(new TabEntry(Transl.get("Opportunity detail"),
                new OpportunityDetailTab(dto, appEnv, opportunityComponentOperation, this, null, false,
                        readOnly, areaTechnologySignsBadgeComponent, listService)));

        OpportunitySubSupplierListTab opportunitySubSupplierListTab = new OpportunitySubSupplierListTab(
                subjectComponentOperation, dto.getId(), appEnv, listService.getSupplierTypeMap());

        tabEntryList.add(new TabEntry(Transl.get("Subsupplier list"),
                opportunitySubSupplierListTab));

        OpportunityOfferTab opportunityOfferTab =
                new OpportunityOfferTab(offerComponentOpperation, appEnv, dto.getId());

        if (SecurityUtils.hasPermission(Permission.OFFER_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Offer list"), opportunityOfferTab));
        }

        OpportunityContactPersonTab opportunityContactPersonTab =
                new OpportunityContactPersonTab(opportunityComponentOperation, dto, appEnv);

        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Contact person list"),
                    opportunityContactPersonTab, Permission.OPPORTUNITY_DOCUMENT_VIEW));
        }

        if (dto.getUser() != null && Objects.equals(SecurityUtils.getCurrentUserId(), dto.getUser().getId())) {
            tabEntryList.add(new TabEntry(Transl.get("Permission"), new CustomPermissionSinglePermissionTabComponent(
                    getChangeAffectedUsersAction(), Permission.OPPORTUNITY_VIEW.name(),
                    DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), userService.findUserList(),
                    dto.getId(), Transl.get("Add read permission to object"))
            ));
        }

        Button addDocument = VaadinComponents.getNewButton(Transl.get("Add new document"), false);
        if (SecurityUtils.hasPermission(Permission.DOCUMENT_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Document list"),
                    new OpportunityDocumentTab(
                            documentComponentOperation, dto, !readOnly, appEnv, addDocument, listService
                    ),
                    Permission.OPPORTUNITY_DOCUMENT_VIEW));
        }

        if (SecurityUtils.hasCustomPermission(DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(),
                dto.getId(), Permission.OPPORTUNITY_EMAIL_VIEW.name())) {
            OpportunityEmailTab opportunityEmailTab = new OpportunityEmailTab(dto, emailComponentOperations, appEnv);
            tabEntryList.add(new TabEntry(Transl.get("Email list"),
                    opportunityEmailTab));
        }

        Button addAreaTechnologySign = VaadinComponents.getNewButton(Transl.get("Add new sign"), false);
        addAreaTechnologySign.addClickListener(buttonClickEvent ->
                areaTechnologyComponentOperation.getAreaTechnologySignEvent(ObjectType.OPPORTUNITY, dto.getId(),
                        areaTechnologySignsBadgeComponent).onComponentEvent(buttonClickEvent));

        String heading = dto.getId() == null ? Transl.get("New opportunity") :
                Transl.get("Opportunity")
                        .concat(" - ")
                        .concat(dto.getName());

        Button addContactButton = VaadinComponents.getLinkButton(Transl.get("Link contact person"));
        addContactButton.addClickListener(e -> {
            Button contactSearch = VaadinComponents.getSearchButton();
            ContactPersonFilterComponent filter = new ContactPersonFilterComponent(
                    contactPersonTypeService.findAllEnabled(), contactSearch);
            String subjectId = dto.getSubject() != null ? dto.getSubject().getId() : "";
            ContactPersonLinkDialog linkDialog = new ContactPersonLinkDialog(
                    contactPersonComponentOperation.getObjectSubjectLinkItemAction(
                            filter, subjectId, dto.getId(), ContactPersonObjectTypeEnum.OPORTUNITY),
                    contactPersonComponentOperation.getLinkContactEvent(
                            opportunityContactPersonTab.getGrid(), dto.getId(),
                            ContactPersonObjectTypeEnum.OPORTUNITY), filter, appEnv);
            contactSearch.addClickListener(event -> linkDialog.loadData());
            linkDialog.open();
        });

        Button linkActivity = VaadinComponents.getLinkButton(Transl.get("Link activity"));
        OpportunityActivityTab opportunityActivityTab =
                new OpportunityActivityTab(dto.getId(), appEnv, activityByObjectComponentOperation);
        tabEntryList.add(new TabEntry(Transl.get("Activity"),
                opportunityActivityTab,
                Permission.ACTIVITY_BY_OBJECT_VIEW));
        linkActivity.addClickListener(buttonClickEvent -> {
            ActivityLinkDto activityLinkDto = new ActivityLinkDto();
            activityLinkDto.setObjectId(dto.getId());
            activityLinkDto.setObjectType(ObjectType.OPPORTUNITY);
            activityLinkDto.setActivityDtoSet(new HashSet<>());
            ActivityLinkDialog activityLinkDialog = new ActivityLinkDialog(listService,
                    activityByObjectComponentOperation.getSaveAction(),
                    opportunityActivityTab.getGrid(), activityLinkDto, appEnv,
                    activityByObjectComponentOperation
                            .getLinkedActivityEmployeeList(dto.getId(), ObjectType.OPPORTUNITY));
            activityLinkDialog.open();
        });

        Button linkEmployee = VaadinComponents.getLinkButton(Transl.get("Link team member"));
        OpportunityEmployeeTab employeeTab = new OpportunityEmployeeTab(
                dto.getId(), appEnv, employeeComponentOperation);
        tabEntryList.add(new TabEntry(Transl.get("Team member"),
                employeeTab,
                Permission.EMPLOYEE_BY_OBJECT_VIEW));
        linkEmployee.addClickListener(buttonClickEvent -> {
            EmployeeLinkDto employeeLinkDto = new EmployeeLinkDto();
            employeeLinkDto.setObjectType(ObjectType.OPPORTUNITY);
            employeeLinkDto.setObjectId(dto.getId());
            employeeLinkDto.setEmployeeDtoSet(new HashSet<>());
            EmployeeLinkDialog employeeLinkDialog = new EmployeeLinkDialog(
                    employeeComponentOperation.getEmployeeByObjectDtoSaveAction(),
                    employeeTab.getGrid(), listService, employeeLinkDto, appEnv,
                    employeeComponentOperation.getLinkedEmployeeList(dto.getId(), ObjectType.OPPORTUNITY));
            employeeLinkDialog.open();
        });

        TaskSlideTabComponent taskSlideTabComponent = new TaskSlideTabComponent(
                taskComponentOperation, appEnv, ObjectType.OPPORTUNITY,
                dto.getId(), dto.getSubject(), listService);

        OpportunityTabsComponent opportunityTabsComponent = new OpportunityTabsComponent(
                heading, tabEntryList, dto, appEnv,
                opportunityComponentOperation,
                addContactButton, !readOnly,
                opportunityOfferTab.getGrid(), offerComponentOpperation,
                userService, listService, subjectComponentOperation,
                opportunitySubSupplierListTab.getGrid(),
                addDocument, entityNewComponentOperation,
                linkActivity, linkEmployee, addAreaTechnologySign, subjectService, contractComponentOperation);
        opportunityTabsComponent.showSubjectLink(dto.getSubject());
        opportunityTabsComponent.addEventSlideTab(taskSlideTabComponent);
        opportunityTabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent
                .Builder(entityNewComponentOperation, EntityNewType.SALES)
                .setSubjectDto(dto.getSubject())
                .setOpportunityDto(dto.getId() != null ? dto : null)
                .setOfferFillSubjectDto(false)
                .setProjectFillSubjectDto(false)
                .setTypeByObjectList(Collections.singletonList(
                        new TypeByObject(dto.getId(), ContactPersonObjectTypeEnum.OPORTUNITY)))
                .build());

        if (dto.getId() != null && SecurityUtils.hasPermission(Permission.OPPORTUNITY_NOTE_VIEW)) {
            Checkbox showArchived = new Checkbox(Transl.get("Show also archived"));
            boolean canViewCustomPermission = SecurityUtils.hasCustomPermission(
                    DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), dto.getId(),
                    Permission.OPPORTUNITY_NOTE_VIEW.name());
            boolean canEditCustomPermission = SecurityUtils.hasCustomPermission(
                    DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), dto.getId(),
                    Permission.OPPORTUNITY_NOTE_EDIT.name());
            CountIntIndicator noteIndicator = new CountIntIndicator(
                    noteComponentOperation.getNoteCountByTypeAndObjectId(NoteTypeEnum.OPPORTUNITY, dto.getId()));
            NoteComponent noteComponent = new NoteComponent(noteComponentOperation, showArchived,
                    dto.getId(), appEnv,
                    NoteTypeEnum.OPPORTUNITY,
                    Permission.OPPORTUNITY_NOTE_VIEW, Permission.OPPORTUNITY_NOTE_EDIT,
                    canViewCustomPermission,
                    canEditCustomPermission, noteIndicator);
            opportunityTabsComponent.addNoteSlideTab(noteComponent, noteIndicator);
        }

        add(opportunityTabsComponent);
    }

    private ChangeAffectedUsersAction getChangeAffectedUsersAction() {
        return userTempSet -> this.userSet = userTempSet;
    }

    private void setParam(String param) throws SystemException {
        if (SecurityUtils.hasCustomPermission(
                DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), param, Permission.OPPORTUNITY_VIEW.name())) {
            dto = opportunityService.getOpportunity(param);
            refreshBreadcrumbText(dto.getName());
            if (SecurityUtils.hasCustomPermission(
                    DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), param, Permission.OPPORTUNITY_EDIT.name())) {
                readOnly = false;
            }
            initView();
        } else {
            ErrorNotification.show(ErrorCode.VIEW_PERMISSION_MISSING.getError(), appEnv);
            UI.getCurrent().access(
                    () -> UI.getCurrent().getPage().fetchCurrentURL(e -> UI.getCurrent().navigate(e.getPath()))
            );
        }
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                setParam(param.replace("&ndash", "/"));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }

    @Override
    public Set<PermUserDto> getSinglePermissionUserList() {
        return userSet;
    }
}