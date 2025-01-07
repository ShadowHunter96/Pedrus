package cz.bbn.cerberus.contract.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.areatechnologysign.ui.AreaTechnologySignsBadgeComponent;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.GridReloadOperation;
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
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.ui.component.ContractTabsComponent;
import cz.bbn.cerberus.contract.ui.component.tab.ContractConnectedContractTab;
import cz.bbn.cerberus.contract.ui.component.tab.ContractContactPersonTab;
import cz.bbn.cerberus.contract.ui.component.tab.ContractDetailTab;
import cz.bbn.cerberus.contract.ui.component.tab.ContractDocumentTab;
import cz.bbn.cerberus.contract.ui.component.tab.ContractEmailTab;
import cz.bbn.cerberus.contract.ui.component.tab.ContractInvoiceGridTab;
import cz.bbn.cerberus.contract.ui.component.tab.ContractProjectTab;
import cz.bbn.cerberus.contract.ui.component.tab.ContractSupplierTab;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.custompermission.ui.ChangeAffectedUsersAction;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.custompermission.ui.component.CustomPermissionSinglePermissionTabComponent;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.ui.component.NoteComponent;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.ProjectComponentOperation;
import cz.bbn.cerberus.project.ui.component.ProjectNewDialog;
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

@Route(value = ContractSalesDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.SALES_CONTRACT_VIEW)
@Slf4j
public class ContractSalesDetailView extends AppView implements HasUrlParameter<String>, CustomPermissionSingleListener {

    public static final String ROUTE = "sales-contract-detail";

    private final ContractService contractService;
    private final AppEnv appEnv;
    private final NoteComponentOperation noteComponentOperation;
    private final DocumentComponentOperation documentComponentOperation;
    private final ContactPersonService contactPersonService;
    private final InvoiceComponentOperation invoiceComponentOperation;
    private final ListService listService;
    private final ContractComponentOperation contractComponentOperation;
    private final ContactPersonTypeService contactPersonTypeService;
    private final ContactPersonComponentOperation contactPersonComponentOperation;
    private final ProjectComponentOperation projectComponentOperation;
    private final AreaTechnologyComponentOperation areaTechnologyComponentOperation;
    private final SubjectComponentOperation subjectComponentOperation;
    private final TaskComponentOperation taskComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final EmailComponentOperations emailComponentOperations;

    private Set<PermUserDto> userSet = new HashSet<>();
    private final UserService userService;

    private ContractContactPersonTab contractContactPersonTab;
    private ContractInvoiceGridTab contractInvoiceGridTab;
    private ContractDto dto;
    private boolean readOnly = true;

    public ContractSalesDetailView(ContractService contractService, AppEnv appEnv,
                                   NoteComponentOperation noteComponentOperation,
                                   DocumentComponentOperation documentComponentOperation,
                                   ContactPersonService contactPersonService,
                                   ContactPersonTypeService contactPersonTypeService,
                                   InvoiceComponentOperation invoiceComponentOperation,
                                   ContractComponentOperation contractComponentOperation,
                                   AreaTechnologyComponentOperation areaTechnologyComponentOperation,
                                   UserService userService,
                                   ContactPersonComponentOperation contactPersonComponentOperation,
                                   ProjectComponentOperation projectComponentOperation,
                                   SubjectComponentOperation subjectComponentOperation, ListService listService,
                                   TaskComponentOperation taskComponentOperation,
                                   EntityNewComponentOperation entityNewComponentOperation,
                                   EmailComponentOperations emailComponentOperations) {
        this.contractService = contractService;
        this.appEnv = appEnv;
        this.noteComponentOperation = noteComponentOperation;
        this.documentComponentOperation = documentComponentOperation;
        this.contactPersonService = contactPersonService;
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.areaTechnologyComponentOperation = areaTechnologyComponentOperation;
        this.listService = listService;
        this.contractComponentOperation = contractComponentOperation;
        this.userService = userService;
        this.contactPersonTypeService = contactPersonTypeService;
        this.contactPersonComponentOperation = contactPersonComponentOperation;
        this.projectComponentOperation = projectComponentOperation;
        this.subjectComponentOperation = subjectComponentOperation;
        this.taskComponentOperation = taskComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.emailComponentOperations = emailComponentOperations;
    }

    private void initView() {
        removeAll();

        AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent = new AreaTechnologySignsBadgeComponent(
                areaTechnologyComponentOperation, ObjectType.CONTRACT, dto.getId());

        List<TabEntry> tabEntryList = new ArrayList<>();

        String heading = dto.getId() == null ? Transl.get("New contract") :
                Transl.get("Contract")
                        .concat(" - ")
                        .concat(dto.getName());

        ComboBox<ContractDto> connectedContract = new ComboBox<>();

        tabEntryList.add(new TabEntry(Transl.get("Contract detail"),
                new ContractDetailTab(dto, contractComponentOperation.getSaveAction(this, null, connectedContract),
                        appEnv, contractService.getMyInvoiceEditContractList(),
                        userService.findUserList(), false, readOnly,
                        contractComponentOperation.findAllowedTypes(dto.getInternalType()),
                        areaTechnologySignsBadgeComponent, listService,
                        connectedContract, contractComponentOperation)));

        ContractProjectTab projectTab = new ContractProjectTab(appEnv,
                projectComponentOperation.getItemsAction(dto.getId(), true),
                listService.getSubjectMap());

        if (SecurityUtils.hasPermission(Permission.PROJECT_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Projects"), projectTab, Permission.PROJECT_VIEW));
        }

        contractContactPersonTab =
                new ContractContactPersonTab(getContactPersonRemoveAction(dto.getId()),
                        getContactPersonItemsAction(dto.getId()),
                        dto, appEnv, contractComponentOperation);

        if (SecurityUtils.hasPermission(Permission.CONTRACT_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Contact person"),
                    contractContactPersonTab, Permission.CONTACT_PERSON_VIEW));
        }

        ContractSupplierTab contractSupplierTab = new ContractSupplierTab(subjectComponentOperation, dto.getId(),
                appEnv, listService.getSupplierTypeMap());

        if (SecurityUtils.hasPermission(Permission.SUPPLIER_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Supplier"), contractSupplierTab, Permission.SUPPLIER_VIEW));
        }

        contractInvoiceGridTab = new ContractInvoiceGridTab(invoiceComponentOperation, dto, appEnv);

        tabEntryList.add(new TabEntry(Transl.get("Connected contracts"), new ContractConnectedContractTab(
                contractComponentOperation.getConnectedContractItemAction(
                        dto.getId(), ContractInternalType.SALES), appEnv)));

        if (SecurityUtils.hasPermission(Permission.INVOICE_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Invoicing"), contractInvoiceGridTab));
        }

        if (dto.getUserDto() != null && Objects.equals(SecurityUtils.getCurrentUserId(), dto.getUserDto().getId())) {
            tabEntryList.add(new TabEntry(Transl.get("Permission"), new CustomPermissionSinglePermissionTabComponent(
                    getChangeAffectedUsersAction(), Permission.CONTRACT_VIEW.name(),
                    DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), userService.findUserList(),
                    dto.getId(), Transl.get("Add read permission to object"))
            ));
        }

        Button addDocument = VaadinComponents.getNewButton(Transl.get("Add document"), false);
        if (SecurityUtils.hasPermission(Permission.DOCUMENT_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Documents"),
                    new ContractDocumentTab(documentComponentOperation, dto, getInvoiceGridReloadOperation(),
                            !readOnly, appEnv, addDocument, listService),
                    Permission.CONTRACT_DOCUMENT_VIEW));
        }

        if (SecurityUtils.hasCustomPermission(DomainEnum.CONTRACT_DOMAIN_NAME.getValue(),
                dto.getId(), Permission.CONTRACT_EMAIL_VIEW.name())) {
            ContractEmailTab contractMailTab = new ContractEmailTab(dto, emailComponentOperations, appEnv);
            tabEntryList.add(new TabEntry(Transl.get("Email list"),
                    contractMailTab));
        }

        Button addAreaTechnologySign = VaadinComponents.getNewButton(Transl.get("Add new sign"), false);
        addAreaTechnologySign.addClickListener(buttonClickEvent ->
                areaTechnologyComponentOperation.getAreaTechnologySignEvent(ObjectType.CONTRACT, dto.getId(),
                        areaTechnologySignsBadgeComponent).onComponentEvent(buttonClickEvent));

        Button createProject = VaadinComponents.getNewButton(Transl.get("Add project"), false);
        createProject.addClickListener(e -> new ProjectNewDialog(
                projectTab.getGrid(), projectComponentOperation, appEnv, null, dto, listService).open());

        Button linkContactPerson = VaadinComponents.getLinkButton(Transl.get("Link contact person"));
        linkContactPerson.addClickListener(e -> {
            Button contactSearch = VaadinComponents.getSearchButton();
            ContactPersonFilterComponent filter = new ContactPersonFilterComponent(
                    contactPersonTypeService.findAllEnabled(), contactSearch);
            String subjectId = dto.getSubjectDto() != null ? dto.getSubjectDto().getId() : "";
            ContactPersonLinkDialog linkDialog = new ContactPersonLinkDialog(
                    contactPersonComponentOperation.getObjectSubjectLinkItemAction(
                            filter, subjectId, dto.getId(), ContactPersonObjectTypeEnum.CONTRACT),
                    contactPersonComponentOperation.getLinkContactEvent(
                            contractContactPersonTab.getGrid(), dto.getId(), ContactPersonObjectTypeEnum.CONTRACT),
                    filter, appEnv);
            contactSearch.addClickListener(event -> linkDialog.loadData());
            linkDialog.open();
        });

        Button linkSupplier = VaadinComponents.getLinkButton(Transl.get("Link supplier"));
        linkSupplier.addClickListener(buttonClickEvent -> {
            SubjectLinkDialog dialog =
                    new SubjectLinkDialog(Transl.get("Link supplier"), subjectComponentOperation,
                            appEnv, dto.getId(), ObjectType.CONTRACT, contractSupplierTab.getGrid());
            dialog.open();
        });

        boolean hasContractNoteView = SecurityUtils.hasCustomPermission(
                DomainEnum.CONTRACT_DOMAIN_NAME.getValue(),
                dto.getId(),
                Permission.CONTRACT_NOTE_VIEW.name()
        );

        ContractTabsComponent tabsComponent =
                new ContractTabsComponent(heading, tabEntryList, createProject,
                        addDocument, linkContactPerson, linkSupplier, addAreaTechnologySign,
                        !readOnly, entityNewComponentOperation, dto);

        tabsComponent.showSubjectLink(dto.getSubjectDto());

        TaskSlideTabComponent taskSlideTabComponent = new TaskSlideTabComponent(
                taskComponentOperation, appEnv, ObjectType.CONTRACT,
                dto.getId(), dto.getSubjectDto(), listService);
        tabsComponent.addEventSlideTab(taskSlideTabComponent);

        List<TypeByObject> typeByObjectList = new ArrayList<>();
        typeByObjectList.add(new TypeByObject(dto.getId(), ContactPersonObjectTypeEnum.CONTRACT));
        typeByObjectList.add(new TypeByObject(dto.getSubjectDto().getId(), ContactPersonObjectTypeEnum.SUBJECT));
        tabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent
                .Builder(entityNewComponentOperation, EntityNewType.SALES)
                .setSubjectDto(dto.getSubjectDto())
                .setOpportunityFillSubjectDto(false)
                .setOfferFillSubjectDto(false)
                .setContractDto(dto)
                .setTypeByObjectList(typeByObjectList).setInvoiceGrid(contractInvoiceGridTab.getGrid())
                .build());

        if (dto.getId() != null && SecurityUtils.hasPermission(Permission.CONTRACT_NOTE_VIEW) && hasContractNoteView) {
            Checkbox showArchived = new Checkbox(Transl.get("Show also archived"));
            boolean canViewCustomPermission = SecurityUtils.hasCustomPermission(
                    DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), dto.getId(), Permission.CONTRACT_NOTE_VIEW.name());
            boolean canEditCustomPermission = SecurityUtils.hasCustomPermission(
                    DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), dto.getId(), Permission.CONTRACT_NOTE_EDIT.name());
            CountIntIndicator noteIndicator = new CountIntIndicator(
                    noteComponentOperation.getNoteCountByTypeAndObjectId(NoteTypeEnum.CONTRACT, dto.getId()));
            NoteComponent noteComponent = new NoteComponent(noteComponentOperation, showArchived,
                    dto.getId(), appEnv,
                    NoteTypeEnum.CONTRACT,
                    Permission.CONTRACT_NOTE_VIEW, Permission.CONTRACT_NOTE_EDIT,
                    canViewCustomPermission, canEditCustomPermission, noteIndicator);
            tabsComponent.addNoteSlideTab(noteComponent, noteIndicator);
        }
        add(tabsComponent);

    }

    private GridReloadOperation getInvoiceGridReloadOperation() {
        return () -> contractInvoiceGridTab.loadData();
    }

    private ChangeAffectedUsersAction getChangeAffectedUsersAction() {
        return userTempSet -> this.userSet = userTempSet;
    }

    private DeleteAction getContactPersonRemoveAction(String objectId) {
        return id -> {
            contactPersonService.deleteAddedContactPerson(id, ContactPersonObjectTypeEnum.CONTRACT, objectId);
            SuccessNotification.showDeleteSuccess(appEnv);
            contractContactPersonTab.loadData();
        };
    }

    private ItemsAction<ContactPersonByObjectDto> getContactPersonItemsAction(String id) {
        return (query, orderList) -> {
            ContactPersonByObjectFilterDto filterDto = new ContactPersonByObjectFilterDto();
            filterDto.setPage(query.getPage());
            filterDto.setSize(query.getPageSize());
            filterDto.setOrderList(orderList);
            filterDto.setObjectType(ContactPersonObjectTypeEnum.CONTRACT);
            filterDto.setObjectId(id);
            return contactPersonService.findSubjectContactPageByObjectPage(filterDto);
        };
    }

    private void setParam(String param) throws SystemException {
        if (SecurityUtils.hasCustomPermission(DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), param,
                Permission.CONTRACT_VIEW.name())) {
            dto = contractService.getContract(param);
            refreshBreadcrumbText(dto.getId());
            if (SecurityUtils.hasCustomPermission(DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), param,
                    Permission.CONTRACT_EDIT.name())) {
                readOnly = false;
            }
            initView();
        } else {
            ErrorNotification.show(Transl.get(ErrorCode.VIEW_PERMISSION_MISSING.getError()), appEnv);
            UI.getCurrent().access(
                    () -> UI.getCurrent().getPage().fetchCurrentURL(e -> UI.getCurrent().navigate(e.getPath()))
            );
        }
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        dto = new ContractDto();
        if (param != null) {
            try {
                setParam(param);
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
