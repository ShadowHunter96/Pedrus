package cz.bbn.cerberus.administration.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.administration.ui.component.AdministrationTabsComponent;
import cz.bbn.cerberus.administration.ui.component.AreaTabComponent;
import cz.bbn.cerberus.administration.ui.component.AssetPositionTabComponent;
import cz.bbn.cerberus.administration.ui.component.ContactPersonTypeTabComponent;
import cz.bbn.cerberus.administration.ui.component.ContractTypeTabComponent;
import cz.bbn.cerberus.administration.ui.component.DocumentTypeTabComponent;
import cz.bbn.cerberus.administration.ui.component.DphTabComponent;
import cz.bbn.cerberus.administration.ui.component.DsSettingTabComponent;
import cz.bbn.cerberus.administration.ui.component.SupplierTypeTabsComponent;
import cz.bbn.cerberus.administration.ui.component.TaskTypeTabComponent;
import cz.bbn.cerberus.administration.ui.component.TechnologyTabComponent;
import cz.bbn.cerberus.area.AreaComponentOperation;
import cz.bbn.cerberus.area.AreaService;
import cz.bbn.cerberus.assetposition.AssetPositionComponentOperation;
import cz.bbn.cerberus.assetposition.AssetPositionService;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.ContactPersonService;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeComponentOperation;
import cz.bbn.cerberus.contactpersontype.ContactPersonTypeService;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.contracttype.ContractTypeComponentOperation;
import cz.bbn.cerberus.contracttype.ContractTypeService;
import cz.bbn.cerberus.document.DocumentService;
import cz.bbn.cerberus.documenttype.DocumentTypeComponentOperation;
import cz.bbn.cerberus.documenttype.DocumentTypeService;
import cz.bbn.cerberus.dph.DphComponentOperation;
import cz.bbn.cerberus.dph.DphService;
import cz.bbn.cerberus.dssetting.DsSettingComponentOperation;
import cz.bbn.cerberus.dssetting.DsSettingService;
import cz.bbn.cerberus.enumeration.EnumerationComponentOperation;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.suppliertype.SupplierTypeComponentOperation;
import cz.bbn.cerberus.suppliertype.SupplierTypeService;
import cz.bbn.cerberus.tasktype.TaskTypeComponentOperation;
import cz.bbn.cerberus.tasktype.TaskTypeService;
import cz.bbn.cerberus.technology.TechnologyComponentOperation;
import cz.bbn.cerberus.technology.TechnologyService;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.List;

@Route(value = AdministrationView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.ADMINISTRATION_VIEW)
public class AdministrationView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "administration";

    // TODO - je tady toho moc. Chce to rozmyslet, jak to rozumne rozdelit a nemit padesat trid v konstruktoru
    private final AppEnv appEnv;
    private final AssetPositionService assetPositionService;
    private final DsSettingService dsSettingService;
    private final AssetPositionComponentOperation assetPositionComponentOperation;
    private final DsSettingComponentOperation dsSettingComponentOperation;
    private final AreaService areaService;
    private final AreaComponentOperation areaComponentOperation;
    private final TechnologyService technologyService;
    private final TechnologyComponentOperation technologyComponentOperation;
    private final EnumerationComponentOperation enumerationComponentOperation;
    private final DphService dphServices;
    private final DphComponentOperation dphComponentOperation;
    private final ContractTypeService contractTypeService;
    private final ContractService contractService;
    private final ContactPersonTypeService contactPersonTypeService;
    private final ContactPersonService contactPersonService;
    private final DocumentTypeService documentTypeService;
    private final SupplierTypeService supplierTypeService;
    private final DocumentService documentService;
    private final ContractTypeComponentOperation contractTypeComponentOperation;
    private final ContactPersonTypeComponentOperation contactPersonTypeComponentOperation;
    private final DocumentTypeComponentOperation documentTypeComponentOperation;
    private final SupplierTypeComponentOperation supplierTypeComponentOperation;
    private final ListService listService;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final TaskTypeService taskTypeService;
    private final TaskTypeComponentOperation taskTypeComponentOperation;

    public AdministrationView(AppEnv appEnv,
                              AssetPositionService assetPositionService, DsSettingService dsSettingService,
                              AssetPositionComponentOperation assetPositionComponentOperation,
                              DsSettingComponentOperation dsSettingComponentOperation,
                              AreaService areaService,
                              AreaComponentOperation areaComponentOperation, TechnologyService technologyService,
                              TechnologyComponentOperation technologyComponentOperation,
                              EnumerationComponentOperation enumerationComponentOperation, DphService dphService,
                              DphComponentOperation dphComponentOperation, ContractTypeService contractTypeService,
                              ContractService contractService, ContactPersonTypeService contactPersonTypeService,
                              ContactPersonService contactPersonService, DocumentTypeService documentTypeService,
                              SupplierTypeService supplierTypeService, DocumentService documentService,
                              ContractTypeComponentOperation contractTypeComponentOperation,
                              ContactPersonTypeComponentOperation contactPersonTypeComponentOperation,
                              DocumentTypeComponentOperation documentTypeComponentOperation,
                              SupplierTypeComponentOperation supplierTypeComponentOperation,
                              ListService listService,
                              EntityNewComponentOperation entityNewComponentOperation, TaskTypeService taskTypeService, TaskTypeComponentOperation taskTypeComponentOperation) {
        this.appEnv = appEnv;
        this.assetPositionService = assetPositionService;
        this.dsSettingService = dsSettingService;
        this.assetPositionComponentOperation = assetPositionComponentOperation;
        this.dsSettingComponentOperation = dsSettingComponentOperation;
        this.areaService = areaService;
        this.areaComponentOperation = areaComponentOperation;
        this.technologyService = technologyService;
        this.technologyComponentOperation = technologyComponentOperation;
        this.enumerationComponentOperation = enumerationComponentOperation;
        this.dphServices = dphService;
        this.dphComponentOperation = dphComponentOperation;
        this.contractTypeService = contractTypeService;
        this.contractService = contractService;
        this.contactPersonTypeService = contactPersonTypeService;
        this.contactPersonService = contactPersonService;
        this.documentTypeService = documentTypeService;
        this.supplierTypeService = supplierTypeService;
        this.documentService = documentService;
        this.contractTypeComponentOperation = contractTypeComponentOperation;
        this.contactPersonTypeComponentOperation = contactPersonTypeComponentOperation;
        this.documentTypeComponentOperation = documentTypeComponentOperation;
        this.supplierTypeComponentOperation = supplierTypeComponentOperation;
        this.listService = listService;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.taskTypeService = taskTypeService;
        this.taskTypeComponentOperation = taskTypeComponentOperation;
    }

    private void initView(int activeTab) {
        removeAll();
        setSizeFull();

        List<TabEntry> tabList = new ArrayList<>();

        if (SecurityUtils.hasPermission(Permission.CONTRACT_TYPE_VIEW)) {
            tabList.add(new TabEntry(Transl.get("Contract type list"),
                    new ContractTypeTabComponent(
                            appEnv, contractTypeService, contractTypeComponentOperation, contractService),
                    Permission.CONTRACT_TYPE_VIEW, ContractTypeTabComponent.TAB_INDEX));
        }

        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_TYPE_VIEW)) {
            tabList.add(new TabEntry(Transl.get("Contact person type list"),
                    new ContactPersonTypeTabComponent(appEnv, contactPersonTypeService,
                            contactPersonService, contactPersonTypeComponentOperation),
                    Permission.CONTACT_PERSON_TYPE_VIEW, ContactPersonTypeTabComponent.TAB_INDEX));
        }

        if (SecurityUtils.hasPermission(Permission.DOCUMENT_TYPE_VIEW)) {
            tabList.add(new TabEntry(Transl.get("Document type list"),
                    new DocumentTypeTabComponent(
                            appEnv, documentTypeService, documentService, documentTypeComponentOperation, listService),
                    Permission.DOCUMENT_TYPE_VIEW, DocumentTypeTabComponent.TAB_INDEX));
        }

        if (SecurityUtils.hasPermission(Permission.SUPPLIER_TYPE_VIEW)) {
            tabList.add(new TabEntry(Transl.get("Supplier type list"),
                    new SupplierTypeTabsComponent(appEnv, supplierTypeService, supplierTypeComponentOperation),
                    Permission.SUPPLIER_TYPE_VIEW, SupplierTypeTabsComponent.TAB_INDEX));
        }

        tabList.add(new TabEntry(Transl.get("Asset position list"),
                new AssetPositionTabComponent(appEnv, assetPositionService, assetPositionComponentOperation),
                Permission.ASSET_POSITION_VIEW, AssetPositionTabComponent.TAB_INDEX));

        if (SecurityUtils.hasPermission(Permission.DS_SETTINGS_VIEW)) {
            tabList.add(new TabEntry(Transl.get("DS setting list"),
                    new DsSettingTabComponent(appEnv, dsSettingService, dsSettingComponentOperation),
                    Permission.DS_SETTINGS_VIEW, DsSettingTabComponent.TAB_INDEX));
        }

        if (SecurityUtils.hasPermission(Permission.AREA_VIEW)) {
            tabList.add(new TabEntry(Transl.get("Area list"),
                    new AreaTabComponent(appEnv, areaService, areaComponentOperation),
                    Permission.AREA_VIEW, AreaTabComponent.TAB_INDEX));
        }

        if (SecurityUtils.hasPermission(Permission.TECHNOLOGY_VIEW)) {
            tabList.add(new TabEntry(Transl.get("Technology list"),
                    new TechnologyTabComponent(appEnv, technologyService, technologyComponentOperation),
                    Permission.TECHNOLOGY_VIEW, TechnologyTabComponent.TAB_INDEX));
        }

        if (SecurityUtils.hasPermission(Permission.DPH_VIEW)) {
            tabList.add(new TabEntry(Transl.get("Dph list"),
                    new DphTabComponent(appEnv, dphServices, dphComponentOperation),
                    Permission.DPH_VIEW, DphTabComponent.TAB_INDEX));
        }

        if (SecurityUtils.hasPermission(Permission.TASK_TYPE_VIEW)) {
            tabList.add(new TabEntry(Transl.get("Task type"),
                    new TaskTypeTabComponent(appEnv, taskTypeService, taskTypeComponentOperation),
                    Permission.TASK_TYPE_VIEW, TaskTypeTabComponent.TAB_INDEX));
        }

        tabList.addAll(enumerationComponentOperation.generateEnumerationTabs(DphTabComponent.TAB_INDEX + 1));

        AdministrationTabsComponent administrationTabsComponent =
                new AdministrationTabsComponent("Administration", tabList, activeTab, entityNewComponentOperation);
        administrationTabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));

        this.add(administrationTabsComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String activeTab) {
        initView(activeTab == null ? ContactPersonTypeTabComponent.TAB_INDEX : Integer.parseInt(activeTab));
    }
}
