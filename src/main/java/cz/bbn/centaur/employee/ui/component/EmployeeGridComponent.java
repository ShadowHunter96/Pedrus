package cz.bbn.cerberus.employee.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.ui.EmployeeDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class EmployeeGridComponent extends AppInfiniteGrid<EmployeeDto> {

    public EmployeeGridComponent(DeleteAction deleteAction, ItemsAction<EmployeeDto> itemsAction, AppEnv appEnv) {
        super(deleteAction, appEnv, itemsAction);
        initGrid();
    }

    private void initGrid() {
        addColumn(EmployeeDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(EmployeeDto::getFirstName).setHeader(Transl.get("First name")).setSortable(true).setKey("firstName");
        addColumn(EmployeeDto::getLastName).setHeader(Transl.get("Last name")).setSortable(true).setKey("lastName");
        addColumn(EmployeeDto::getCompanyEmail).setHeader(Transl.get("Company email"))
                .setSortable(true).setKey("companyEmail");
        addColumn(EmployeeDto::getCompanyPhoneNumber).setHeader(Transl.get("Company phone number"))
                .setSortable(true).setKey("companyPhoneNumber");
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));

    }

    private HorizontalLayout getGridButtons(EmployeeDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit employee",
                "Are you sure you want to delete employee {0} ?", "Delete employee");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.EMPLOYEE_VIEW, Permission.EMPLOYEE_DELETE, clickedItem.getId(),
                clickedItem.getFirstName() + " " + clickedItem.getLastName(), EmployeeDetailView.ROUTE,
                "", clickedItem.getDeleted());
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(EmployeeDetailView.ROUTE + "/" + code);
    }

}
