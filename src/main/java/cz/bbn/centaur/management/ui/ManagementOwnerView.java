package cz.bbn.cerberus.management.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.management.ManagementComponentOperation;
import cz.bbn.cerberus.management.OwnerService;
import cz.bbn.cerberus.management.ui.component.OwnerDialogComponent;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.permissionmanagement.ui.component.ManagementOwnerComponent;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = ManagementOwnerView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.MANAGEMENT_OWNER_VIEW)
@Slf4j
public class ManagementOwnerView extends AppView {

    public static final String ROUTE = "management-owner";

    private final EntityNewComponentOperation entityNewComponentOperation;
    private final ManagementComponentOperation managementComponentOperation;
    private final ListService listService;
    private final OwnerService ownerService;
    private final AppEnv appEnv;

    public ManagementOwnerView(EntityNewComponentOperation entityNewComponentOperation,
                               ManagementComponentOperation managementComponentOperation, ListService listService,
                               OwnerService ownerService, AppEnv appEnv) {
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.managementComponentOperation = managementComponentOperation;
        this.listService = listService;
        this.ownerService = ownerService;
        this.appEnv = appEnv;
        initView();
    }

    private void initView(){
        removeAll();
        setSizeFull();
        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Owner management"), entityNewComponentOperation);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));

        ManagementOwnerComponent managementUserPermissionsComponent = new ManagementOwnerComponent(listService, ownerService, appEnv);
        card.add(managementUserPermissionsComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.addClickListener(buttonClickEvent ->
                managementComponentOperation.getSaveAction(managementUserPermissionsComponent.getObjectType(), managementUserPermissionsComponent.getUser())
                .saveItem(managementUserPermissionsComponent.getOwnerGridComponent2().getDataForSave(), null));
        Button transfer = VaadinComponents.getButton(Transl.get("Bulk owner transfer"));
        transfer.addClickListener(buttonClickEvent -> {
            OwnerDialogComponent ownerDialogComponent = new OwnerDialogComponent(listService.getUserDtoList(), managementComponentOperation, appEnv);
            ownerDialogComponent.open();
        });
        card.addButtons(transfer, submit);
        card.showFooter(true);
        this.add(card);
    }
}
