package cz.bbn.cerberus.employee.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeByObjectDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class EmployeeByObjectGridComponent extends AppInfiniteGrid<EmployeeByObjectDto> {

    public EmployeeByObjectGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                         ItemsAction<EmployeeByObjectDto> itemsAction) {
        super(deleteAction, appEnv, itemsAction);
        initGrid();
    }

    private void initGrid() {
        addColumn(employeeByObjectDto -> employeeByObjectDto.getEmployeeDto().getId())
                .setHeader(Transl.get("ID")).setSortable(true).setKey("employeeEntity.id");
        addColumn(employeeByObjectDto -> employeeByObjectDto.getEmployeeDto().getFirstName())
                .setHeader(Transl.get("First name")).setSortable(true).setKey("employeeEntity.firstName");
        addColumn(employeeByObjectDto -> employeeByObjectDto.getEmployeeDto().getLastName())
                .setHeader(Transl.get("Last name")).setSortable(true).setKey("employeeEntity.lastName");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_SMALL)
                .setFlexGrow(1).setTextAlign(ColumnTextAlign.CENTER);
    }

    private HorizontalLayout getGridButtons(EmployeeByObjectDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_BY_OBJECT_UNLINK)) {
            Button unlink = VaadinComponents.getUnlinkButton();
            AppUtils.addRfClassToGridButton(unlink, String.valueOf(clickedItem.getId()));
            unlink.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getId().getEmployeeId()),
                                Transl.get("Are you sure you want to unlink team member {0} ?",
                                        String.valueOf(clickedItem.getEmployeeDto().getFirstName())),
                                getAppEnv(), true);
                deleteConfirmDialog.open();
            });
            unlink.getElement().setProperty(TextValues.TITLE, Transl.get("Unlink team member"));
            buttons.add(unlink);
        }

        return buttons;
    }
}
