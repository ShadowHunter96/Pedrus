package cz.bbn.cerberus.user.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.azure.AzureGraphService;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.ui.UserDetailView;

public class UserGridComponent extends AppInfiniteGrid<UserDto> {

    private final AzureGraphService azureGraphService;
    private final EmployeeComponentOperation employeeComponentOperation;

    public UserGridComponent(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<UserDto> itemsAction,
                             AzureGraphService azureGraphService,
                             EmployeeComponentOperation employeeComponentOperation) {
        super(deleteAction, appEnv, itemsAction);
        this.azureGraphService = azureGraphService;
        this.employeeComponentOperation = employeeComponentOperation;
        initGrid();
    }

    private void initGrid() {
        this.setSizeFull();
        addColumn(UserDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(UserDto::getLogin).setHeader(Transl.get("Login name")).setSortable(true).setKey("login");
        addColumn(UserDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(UserDto::getAcronym).setHeader(Transl.get("Acronym")).setSortable(true).setKey("acronym");
        addColumn(UserDto::getMail).setHeader(Transl.get("Mail")).setSortable(true).setKey("mail");
        addColumn(new ComponentRenderer<>(this::getEmployeeName)).setHeader(Transl.get("Employee")).setKey("employee");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event ->
                UI.getCurrent().navigate(UserDetailView.ROUTE + "/" + event.getItem().getId()));
    }

    private HorizontalLayout getGridButtons(UserDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit user",
                "Are you sure you want to delete user {0} ?", "Delete user");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.USER_EDIT, Permission.USER_DELETE, String.valueOf(clickedItem.getId()),
                clickedItem.getName(),
                UserDetailView.ROUTE, clickedItem.getDeleted());
        HorizontalLayout layout = getSimpleGridButtons(dataVariables, stringVariables, this);
        Button sendTeamsMessage = VaadinComponents.getButton(VaadinIcon.CHAT.create());
        sendTeamsMessage.getElement().setProperty(TextValues.TITLE, Transl.get("Send teams message"));
        sendTeamsMessage.addClickListener(buttonClickEvent -> {
            TeamsMessageDialogComponent dialog =
                    new TeamsMessageDialogComponent(azureGraphService, clickedItem);
            dialog.open();
        });
        layout.add(sendTeamsMessage);
        if (SecurityUtils.getCurrentUserDto().getId().equals(clickedItem.getId())) {
            sendTeamsMessage.setClassName("button-hidden");
        }
        return layout;
    }

    private Span getEmployeeName(UserDto userDto) {
        return new Span(employeeComponentOperation.getEmployeeName(userDto.getEmployee()));
    }
}
