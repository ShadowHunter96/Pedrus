package cz.bbn.cerberus.invoice.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.invoice.ui.component.InvoiceDtoGridComponent;
import cz.bbn.cerberus.invoice.ui.component.InvoiceFilterComponent;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;


@Route(value = InvoicingView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.INVOICE_VIEW)
@Slf4j
public class InvoicingView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "invoice-list";

    private final ListService listService;
    private final InvoiceComponentOperation invoiceComponentOperation;
    private final AppEnv appEnv;
    private final UserService userService;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public InvoicingView(ListService listService, InvoiceComponentOperation invoiceComponentOperation,
                         AppEnv appEnv, UserService userService,
                         EntityNewComponentOperation entityNewComponentOperation) {
        this.listService = listService;
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.appEnv = appEnv;
        this.userService = userService;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();
        Button search = VaadinComponents.getSearchButton();
        InvoiceFilterComponent invoiceFilterComponent = new InvoiceFilterComponent(userService.findUserList(),
                listService.getContractList(), listService.getSubjectDtoList(), search, params,
                getHistoryBreadcrumbs());

        InvoiceDtoGridComponent grid = new InvoiceDtoGridComponent(invoiceComponentOperation,
                invoiceFilterComponent, listService.getContractList(), appEnv);

        AppCardGridComponent card =  new AppCardGridComponent(Transl.get("Invoice list"),
                Permission.INVOICE_EDIT,
                Transl.get("Add invoice"),
                entityNewComponentOperation.getInvoiceEvent(grid, null, null),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);

        card.setId(RobotFrameworkVariables.INVOICE_VIEW_CARD_ID.getValue());
        card.add(invoiceFilterComponent);
        card.add(grid);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            invoiceFilterComponent.fillUrl();
        });
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
