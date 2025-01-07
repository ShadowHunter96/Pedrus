package cz.bbn.cerberus.employee.ui;

import com.vaadin.flow.component.button.Button;
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
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.employee.ui.component.EmployeeFilterComponent;
import cz.bbn.cerberus.employee.ui.component.EmployeeGridComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;


@Route(value = EmployeeView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.EMPLOYEE_VIEW)
@Slf4j
public class EmployeeView extends AppView {

    public static final String ROUTE = "employee-list";

    private final EmployeeComponentOperation employeeComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final AppEnv appEnv;

    public EmployeeView(EmployeeComponentOperation employeeComponentOperation,
                        EntityNewComponentOperation entityNewComponentOperation,
                        AppEnv appEnv) {
        this.employeeComponentOperation = employeeComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.appEnv = appEnv;
        initView();
    }

    private void initView() {
        removeAll();

        Button search = VaadinComponents.getSearchButton();
        EmployeeFilterComponent employeeFilterComponent = new EmployeeFilterComponent(search);

        EmployeeGridComponent grid = new EmployeeGridComponent(
                employeeComponentOperation.getDeleteAction(),
                employeeComponentOperation.getEmployeeDtoItemsAction(employeeFilterComponent), appEnv);

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Employee list"),
                Permission.EMPLOYEE_EDIT, Transl.get("Add employee"),
                entityNewComponentOperation.getNewEmployeeDialogEvent(grid), entityNewComponentOperation,
                NoteTypeEnum.ANY, ObjectType.ANY);
        card.setId(RobotFrameworkVariables.EMPLOYEE_VIEW_CARD_ID.getValue());
        card.add(employeeFilterComponent);
        card.add(grid);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.BACKOFFICE));
        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent ->
                grid.loadData()
        );
    }

}
