package cz.bbn.cerberus.employeecontract.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.GridSortOrder;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.provider.SortDirection;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employeecontract.EmployeeContractComponentOperation;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.ui.EmployeeContractDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.List;

public class EmployeeContractGridComponent extends AppInfiniteGrid<EmployeeContractDto> {

    private final EmployeeContractComponentOperation componentOperation;
    private final boolean simpleGrid;
    private final EmployeeContractFilterDtoComponent employeeContractFilterComponent;

    public EmployeeContractGridComponent(AppEnv appEnv, ItemsAction<EmployeeContractDto> itemsAction,
                                         EmployeeContractComponentOperation componentOperation,
                                         EmployeeContractFilterDtoComponent employeeContractFilterComponent) {
        super(appEnv, itemsAction);
        this.componentOperation = componentOperation;
        this.employeeContractFilterComponent = employeeContractFilterComponent;
        this.simpleGrid = false;
        initGrid();
    }

    public EmployeeContractGridComponent(AppEnv appEnv, ItemsAction<EmployeeContractDto> itemsAction) {
        super(appEnv, itemsAction);
        this.employeeContractFilterComponent = null;
        this.componentOperation = null;
        this.simpleGrid = true;
        initGrid();
    }

    private void initGrid() {
        addColumn(EmployeeContractDto::getId).setHeader(Transl.get("Contract number")).setSortable(true).setKey("id");
        addColumn(new ComponentRenderer<>(this::getEmployee)).setHeader(Transl.get("Employee"))
                .setSortable(true).setKey("employee.id");
        addColumn(empDto -> Transl.get(empDto.getState().getName())).setHeader(Transl.get("State"))
                .setSortable(true).setKey("state");
        addColumn(empDto -> empDto.getType().getName()).setHeader(Transl.get("Type"))
                .setSortable(true).setKey("type.id");
        addColumn(empDto -> empDto.getOwnCompany().getName()).setHeader(Transl.get("Company"))
                .setSortable(true).setKey("ownCompany.name");
        Column<EmployeeContractDto> creationDate = addColumn(
                EmployeeContractDto::getCreationDate).setKey("creationDate");
        creationDate.setVisible(false);

        GridSortOrder<EmployeeContractDto> sortOrder = new GridSortOrder<>(creationDate, SortDirection.DESCENDING);
        List<GridSortOrder<EmployeeContractDto>> sortList = new ArrayList<>();
        sortList.add(sortOrder);
        sort(sortList);

        if ((SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_DELETE)
                || SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_ARCHIVE)) && !simpleGrid) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);
        }

        addItemDoubleClickListener(
                e -> UI.getCurrent().navigate(EmployeeContractDetailView.ROUTE + "/" + e.getItem().getId()));
    }

    private Span getEmployee(EmployeeContractDto dto) {
        if (dto.getEmployee() != null) {
            return new Span(dto.getEmployee().getFirstName() + " " + dto.getEmployee().getLastName());
        }
        return new Span();
    }

    private HorizontalLayout getGridButtons(EmployeeContractDto dto) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete"));
            delete.addClickListener(e -> componentOperation.getDeleteConfirmDialog(dto));
            buttons.add(delete);
        }

        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_ARCHIVE) && !dto.getArchived()) {
            Button archive = VaadinComponents.getButton(VaadinIcon.ARCHIVE.create());
            archive.getElement().setProperty(TextValues.TITLE, Transl.get("Archive"));
            archive.addClickListener(e -> componentOperation.archive(dto, true, employeeContractFilterComponent.getParamUrl()));
            buttons.add(archive);
        } else if(SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_UNARCHIVE) && dto.getArchived()){
            Button unarchive = VaadinComponents.getButton(VaadinIcon.ARROW_FORWARD.create());
            unarchive.getElement().setProperty(TextValues.TITLE, Transl.get("Unarchive"));
            unarchive.addClickListener(e -> componentOperation.archive(dto, false,
                    employeeContractFilterComponent.getParamUrl()));
            buttons.add(unarchive);
        }
        return buttons;
    }
}
