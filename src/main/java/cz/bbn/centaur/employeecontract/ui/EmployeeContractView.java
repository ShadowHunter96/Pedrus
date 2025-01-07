package cz.bbn.cerberus.employeecontract.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.employeecontract.EmployeeContractComponentOperation;
import cz.bbn.cerberus.employeecontract.ui.component.EmployeeContractFilterDtoComponent;
import cz.bbn.cerberus.employeecontract.ui.component.EmployeeContractGridComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = EmployeeContractView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.EMPLOYEE_CONTRACT_VIEW)
@Slf4j
public class EmployeeContractView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "employee-contract-list";

    private final EntityNewComponentOperation entityNewComponentOperation;
    private final EmployeeContractComponentOperation componentOperation;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final AppEnv appEnv;

    public EmployeeContractView(EntityNewComponentOperation entityNewComponentOperation,
                                EmployeeContractComponentOperation componentOperation,
                                HistoryBreadcrumbs historyBreadcrumbs, AppEnv appEnv) {
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.componentOperation = componentOperation;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.appEnv = appEnv;
    }

    private void initView(String params) {
        removeAll();

        Button search = VaadinComponents.getSearchButton();
        EmployeeContractFilterDtoComponent employeeContractFilterComponent = new EmployeeContractFilterDtoComponent(
                search, componentOperation.getEmployeeList(), componentOperation.getStateList(),
                componentOperation.getTypeList(), componentOperation.getOwnCompanyList(), params, historyBreadcrumbs);

        EmployeeContractGridComponent grid = new EmployeeContractGridComponent(
                appEnv, componentOperation.getItemsAction(employeeContractFilterComponent), componentOperation, employeeContractFilterComponent);

        AppCardGridComponent card = new AppCardGridComponent(
                Transl.get("Employee contract list"), Permission.EMP_CONTRACT_STATE_EDIT,
                Transl.get("Add employee contract"), componentOperation.getEmployeeContractNewEvent(grid),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.BACKOFFICE));
        card.setId(RobotFrameworkVariables.EMPLOYEE_CONTRACT_VIEW_CARD_ID.getValue());
        card.add(employeeContractFilterComponent);
        card.add(grid);

        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            employeeContractFilterComponent.fillUrl();
        });
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
