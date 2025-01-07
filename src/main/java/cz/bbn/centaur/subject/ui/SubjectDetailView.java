package cz.bbn.cerberus.subject.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.applog.dto.AppLogDto;
import cz.bbn.cerberus.applog.dto.AppLogFilterDto;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
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
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.custompermission.ui.ChangeAffectedUsersAction;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.custompermission.ui.component.CustomPermissionSinglePermissionTabComponent;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.label.LabelComponentOperation;
import cz.bbn.cerberus.label.ui.component.LabelLinkDialog;
import cz.bbn.cerberus.labelsubject.LabelSubjectComponentOperation;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.ui.component.NoteComponent;
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.opportunity.OpportunityService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDtoFilter;
import cz.bbn.cerberus.opportunity.dto.OpportunitySimpleDto;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.ProjectComponentOperation;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.component.SubjectTabsComponent;
import cz.bbn.cerberus.subject.ui.component.ChangeSubjectTypeDialog;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectChangeLogTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectContactPersonTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectContractTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectDetailTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectDocumentTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectEmailTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectInfoTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectInvoiceGridTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectOfferTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectOpportunityTab;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectProjectTab;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.ui.component.TaskSlideTabComponent;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;

@Route(value = SubjectDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.SUBJECT_VIEW)
@Slf4j
public class SubjectDetailView extends AppView implements HasUrlParameter<String>, CustomPermissionSingleListener {

    public static final String ROUTE = "subject-detail";

    private SubjectContactPersonTab subjectContactPersonTab;
    private SubjectDto dto;

    private final SubjectService subjectService;
    private final AppEnv appEnv;
    private final NoteComponentOperation noteComponentOperation;
    private final ContactPersonService contactPersonService;
    private final DocumentComponentOperation documentComponentOperation;
    private final OpportunityService opportunityService;
    private final ListService listService;
    private final AppLogService appLogService;
    private final ContactPersonTypeService contactPersonTypeService;
    private final SubjectComponentOperation subjectComponentOperation;
    private final ProjectComponentOperation projectComponentOperation;
    private final ContactPersonComponentOperation contactPersonComponentOperation;
    private final ContractComponentOperation contractComponentOperation;
    private final InvoiceComponentOperation invoiceComponentOperation;
    private final LabelSubjectComponentOperation labelSubjectComponentOperation;
    private final LabelComponentOperation labelComponentOperation;
    private final OfferComponentOpperation offerComponentOpperation;
    private final TaskComponentOperation taskComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final EmailComponentOperations emailComponentOperations;

    private Set<PermUserDto> userSet;
    private boolean readOnly = true;

    public SubjectDetailView(SubjectService subjectService, AppEnv appEnv, NoteComponentOperation noteComponentOperation,
                             ContactPersonService contactPersonService,
                             DocumentComponentOperation documentComponentOperation,
                             ListService listService, OpportunityService opportunityService,
                             AppLogService appLogService,
                             SubjectComponentOperation subjectComponentOperation,
                             ProjectComponentOperation projectComponentOperation,
                             ContactPersonComponentOperation contactPersonComponentOperation,
                             ContractComponentOperation contractComponentOperation,
                             ContactPersonTypeService contactPersonTypeService,
                             InvoiceComponentOperation invoiceComponentOperation,
                             LabelSubjectComponentOperation labelSubjectComponentOperation,
                             LabelComponentOperation labelComponentOperation,
                             OfferComponentOpperation offerComponentOpperation,
                             TaskComponentOperation taskComponentOperation,
                             EntityNewComponentOperation entityNewComponentOperation,
                             EmailComponentOperations emailComponentOperations) {
        this.subjectService = subjectService;
        this.appEnv = appEnv;
        this.noteComponentOperation = noteComponentOperation;
        this.contactPersonService = contactPersonService;
        this.documentComponentOperation = documentComponentOperation;
        this.opportunityService = opportunityService;
        this.listService = listService;
        this.appLogService = appLogService;
        this.subjectComponentOperation = subjectComponentOperation;
        this.contactPersonTypeService = contactPersonTypeService;
        this.projectComponentOperation = projectComponentOperation;
        this.contactPersonComponentOperation = contactPersonComponentOperation;
        this.contractComponentOperation = contractComponentOperation;
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.labelSubjectComponentOperation = labelSubjectComponentOperation;
        this.labelComponentOperation = labelComponentOperation;
        this.offerComponentOpperation = offerComponentOpperation;
        this.taskComponentOperation = taskComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.emailComponentOperations = emailComponentOperations;
    }

    private void initView(SubjectDto dto) {
        setSizeFull();
        removeAll();
        List<TabEntry> tabEntryList = new ArrayList<>();
        String ownCompany = subjectComponentOperation.getOwnNameCompany();
        SubjectDetailTab subjectDetailTab = new SubjectDetailTab(dto, subjectComponentOperation, appEnv,
                null, this, false,
                readOnly, listService.getSupplierTypeDtoList(),
                ownCompany != null && !ownCompany.isEmpty(), ownCompany);
        tabEntryList.add(new TabEntry(
                Transl.get("Subject detail"),
                subjectDetailTab));

        SubjectInfoTab subjectInfoTab = new SubjectInfoTab(dto, appEnv, labelSubjectComponentOperation);
        tabEntryList.add(new TabEntry(
                Transl.get("Subject info"),
                subjectInfoTab
        ));

        String heading = dto.getId() == null ? getNewSubjectHeading(dto.getLocalSubject()) :
                Transl.get("Subject")
                        .concat(" - ")
                        .concat(dto.getName());


        Button linkContactPersonButton = VaadinComponents.getLinkButton(Transl.get("Link contact person"));

        Button linkLabelButton = VaadinComponents.getLinkButton(Transl.get("Link label"));

        subjectContactPersonTab =
                new SubjectContactPersonTab(
                        getContactPersonRemoveAction(), getContactPersonItemsAction(), appEnv
                );
        SubjectOpportunityTab subjectOpportunityTab = new SubjectOpportunityTab(getItemsAction(dto), appEnv,
                listService.getSubjectMap(), subjectComponentOperation);
        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Contact person"), subjectContactPersonTab));
        }
        if (SecurityUtils.hasPermission(Permission.OPPORTUNITY_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Opportunity list"), subjectOpportunityTab));
        }

        SubjectOfferTab subjectOfferTab = new SubjectOfferTab(offerComponentOpperation, appEnv, dto.getId());
        if (SecurityUtils.hasPermission(Permission.OFFER_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Offer list"), subjectOfferTab));
        }

        SubjectContractTab subjectContractTab = new SubjectContractTab(contractComponentOperation, appEnv, dto);
        if (SecurityUtils.hasPermission(Permission.CONTRACT_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Contract list"), subjectContractTab));
        }

        SubjectInvoiceGridTab subjectInvoiceGridTab = new SubjectInvoiceGridTab(
                invoiceComponentOperation, appEnv, contractComponentOperation.getContractListBySubject(dto.getId()));
        if (SecurityUtils.hasPermission(Permission.INVOICE_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Invoicing"), subjectInvoiceGridTab));
        }

        SubjectProjectTab subjectProjectTab = new SubjectProjectTab(
                appEnv, projectComponentOperation.getItemsAction(dto.getId(), false), listService.getSubjectMap());
        tabEntryList.add(new TabEntry(Transl.get("Projects"),
                subjectProjectTab));

        Button addDocument = VaadinComponents.getNewButton(Transl.get("Add new document"), false);

        if (dto.getUserDto() != null && dto.getUserDto().getId().equals(SecurityUtils.getCurrentUserId())) {
            if (SecurityUtils.hasPermission(Permission.DOCUMENT_VIEW)) {
                tabEntryList.add(new TabEntry(Transl.get("Documents"),
                        new SubjectDocumentTab(documentComponentOperation, dto, !readOnly, appEnv,
                                addDocument, listService), Permission.SUBJECT_DOCUMENT_VIEW));
            }
        }

        if (SecurityUtils.hasCustomPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue(),
                dto.getId(), Permission.SUBJECT_EMAIL_VIEW.name())) {
            SubjectEmailTab subjectEmailTab = new SubjectEmailTab(dto, emailComponentOperations, appEnv);
            tabEntryList.add(new TabEntry(Transl.get("Email list"),
                    subjectEmailTab));
        }

        if (dto.getUserDto() != null && dto.getUserDto().getId().equals(SecurityUtils.getCurrentUserId())) {
            tabEntryList.add(new TabEntry(Transl.get("Subject change log"),
                    new SubjectChangeLogTab(getLogsItemAction(), appEnv)));

            if (Objects.equals(SecurityUtils.getCurrentUserId(), dto.getUserDto().getId())) {
                tabEntryList.add(new TabEntry(Transl.get("Permission"), new CustomPermissionSinglePermissionTabComponent(
                        getChangeAffectedUsersAction(), Permission.SUBJECT_VIEW.name(),
                        DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), subjectComponentOperation.getUserList(), dto.getId(),
                        Transl.get("Add read permission to object"))
                ));
            }
        }

        linkContactPersonButton.addClickListener(e -> {
            Button contactSearch = VaadinComponents.getSearchButton();
            ContactPersonFilterComponent filter =
                    new ContactPersonFilterComponent(contactPersonTypeService.findAllEnabled(), contactSearch);
            ContactPersonLinkDialog linkDialog = new ContactPersonLinkDialog(
                    contactPersonComponentOperation.getSubjectLinkItemAction(filter, dto.getId()),
                    contactPersonComponentOperation.getLinkContactEvent(subjectContactPersonTab.getGrid(), dto.getId(),
                            ContactPersonObjectTypeEnum.SUBJECT),
                    filter, appEnv);
            contactSearch.addClickListener(event -> linkDialog.loadData());
            linkDialog.open();
        });

        linkLabelButton.addClickListener(buttonClickEvent -> {
            LabelLinkDialog dialog = new LabelLinkDialog(appEnv, labelComponentOperation, dto.getId(),
                    subjectInfoTab.getGrid(), labelSubjectComponentOperation);
            dialog.open();
        });

        Button changeSubjectType = new Button(Transl.get("Change subject type"));
        changeSubjectType.addClickListener(buttonClickEvent -> {
            ChangeSubjectTypeDialog dialog = new ChangeSubjectTypeDialog(dto, subjectComponentOperation.getUpdateAction(), appEnv);
            dialog.open();
        });

        SubjectTabsComponent subjectTabsComponent = new SubjectTabsComponent(
                heading, tabEntryList, linkContactPersonButton, linkLabelButton,
                changeSubjectType, !readOnly,
                addDocument, getIcoButton(dto, subjectDetailTab.getBinder()),
                entityNewComponentOperation, dto);

        boolean canEditCustomPermission = SecurityUtils.hasCustomPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue(),
                dto.getId(), Permission.SUBJECT_EDIT.name());

        TaskSlideTabComponent taskSlideTabComponent = new TaskSlideTabComponent(taskComponentOperation, appEnv,
                ObjectType.SUBJECT, dto.getId(), dto, listService);

        subjectTabsComponent.addEventSlideTab(taskSlideTabComponent);
        subjectTabsComponent.addNewEntitySlideTab(
                new NewEntityButtonsComponent.Builder(entityNewComponentOperation, EntityNewType.SALES)
                        .setSubjectDto(dto)
                        .setTypeByObjectList(Collections.singletonList(
                                new TypeByObject(dto.getId(), ContactPersonObjectTypeEnum.SUBJECT)))
                        .build());

        if (SecurityUtils.hasPermission(Permission.SUBJECT_NOTE_VIEW)) {
            Checkbox showArchived = new Checkbox(Transl.get("Show also archived"));
            boolean canViewCustomPermission = SecurityUtils.hasCustomPermission(
                    DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), dto.getId(), Permission.SUBJECT_NOTE_VIEW.name());
            CountIntIndicator noteIndicator = new CountIntIndicator(
                    noteComponentOperation.getNoteCountByTypeAndObjectId(NoteTypeEnum.SUBJECT, dto.getId()));
            NoteComponent noteComponent = new NoteComponent(noteComponentOperation, showArchived,
                    dto.getId(), appEnv,
                    NoteTypeEnum.SUBJECT,
                    Permission.SUBJECT_NOTE_VIEW, Permission.SUBJECT_NOTE_EDIT,
                    canViewCustomPermission, canEditCustomPermission, noteIndicator);
            subjectTabsComponent.addNoteSlideTab(noteComponent, noteIndicator);
        }
        add(subjectTabsComponent);
    }

    private Button getIcoButton(SubjectDto subjectDto, Binder<SubjectDto> binder) {
        Button icoButton = VaadinComponents.getButton(Transl.get("Update by ICO"));
        icoButton.setIcon(VaadinIcon.REFRESH.create());
        icoButton.addClassName(RobotFrameworkVariables.ICO_BUTTON_CLASS.getValue());
        icoButton.addClickListener(e -> {
            if (subjectDto.getId() != null && !"".equals(subjectDto.getId().trim())) {
                subjectComponentOperation.updateDataFromAres(subjectDto);
                binder.setBean(subjectDto);
            }
        });
        return icoButton;
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                setParam(param);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }

    private void setParam(String param) throws SystemException {
        if (SecurityUtils.hasCustomPermission(
                DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), param, Permission.SUBJECT_VIEW.name())) {
            dto = subjectService.getSubjectDto(param);
            refreshBreadcrumbText(dto.getId());
            if (SecurityUtils.hasCustomPermission(
                    DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), param, Permission.SUBJECT_EDIT.name())) {
                readOnly = false;
            }
            initView(dto);
        } else {
            ErrorNotification.show(ErrorCode.VIEW_PERMISSION_MISSING.getError(), appEnv);

            UI.getCurrent().access(
                    () -> UI.getCurrent().getPage().fetchCurrentURL(e -> UI.getCurrent().navigate(e.getPath()))
            );
        }
    }

    private ChangeAffectedUsersAction getChangeAffectedUsersAction() {
        return userTempSet -> this.userSet = userTempSet;
    }

    private DeleteAction getContactPersonRemoveAction() {
        return id -> {
            contactPersonService.deleteAddedContactPerson(id, ContactPersonObjectTypeEnum.SUBJECT, dto.getId());
            SuccessNotification.showDeleteSuccess(appEnv);
            subjectContactPersonTab.loadData();
        };
    }

    private ItemsAction<ContactPersonByObjectDto> getContactPersonItemsAction() {
        return (query, orderList) -> {
            ContactPersonByObjectFilterDto filterDto = new ContactPersonByObjectFilterDto();
            filterDto.setPage(query.getPage());
            filterDto.setSize(query.getPageSize());
            filterDto.setOrderList(orderList);
            filterDto.setObjectType(ContactPersonObjectTypeEnum.SUBJECT);
            filterDto.setObjectId(dto.getId());
            return contactPersonService.findSubjectContactPageByObjectPage(filterDto);
        };
    }

    private ItemsAction<OpportunitySimpleDto> getItemsAction(SubjectDto dto) {
        return (query, orderList) -> {
            OpportunityDtoFilter filter = new OpportunityDtoFilter();
            filter.setSubjectId(dto.getId());
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return opportunityService.findOpportunityDtoPage(filter);
        };
    }


    private ItemsAction<AppLogDto> getLogsItemAction() {
        return (query, orderList) -> {
            AppLogFilterDto filterDto = new AppLogFilterDto();
            filterDto.setAppId(dto.getId());
            filterDto.setPage(query.getPage());
            filterDto.setSize(query.getPageSize());
            if (orderList.isEmpty()) {
                orderList.add(Sort.Order.desc("date"));
            }
            filterDto.setOrderList(orderList);
            return appLogService.findAppLogDtoPage(filterDto);
        };
    }

    private String getNewSubjectHeading(Boolean localSubject) {
        if (Boolean.TRUE.equals(localSubject)) {
            return Transl.get("Add CZ subject");
        }
        return Transl.get("Add foreign subject");
    }

    @Override
    public Set<PermUserDto> getSinglePermissionUserList() {
        return userSet;
    }

}
