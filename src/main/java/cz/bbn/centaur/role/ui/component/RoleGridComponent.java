package cz.bbn.cerberus.role.ui.component;

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
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.role.ui.RoleDetailView;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class RoleGridComponent extends AppInfiniteGrid<RoleEntity> {

    public RoleGridComponent(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<RoleEntity> itemAction) {
        super(deleteAction, appEnv, itemAction);
    }

    public void initGrid() {
        addColumn(RoleEntity::getId).setAutoWidth(true).setHeader(Transl.get("Role code"))
                .setSortable(true).setKey("id");
        addColumn(RoleEntity::getName).setAutoWidth(true).setHeader(Transl.get("Role description"))
                .setSortable(true).setKey("description");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));

    }

    private HorizontalLayout getGridButtons(RoleEntity clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit role",
                "Are you sure you want to delete role {0} ?", "Delete role");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.ROLE_VIEW, Permission.ROLE_DELETE, clickedItem.getId(), "", RoleDetailView.ROUTE, false);
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(RoleDetailView.ROUTE + "/" + code);
    }
}
