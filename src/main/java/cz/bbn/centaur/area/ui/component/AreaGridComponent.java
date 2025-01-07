package cz.bbn.cerberus.area.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.area.ui.AreaDetailView;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class AreaGridComponent extends AppInfiniteGrid<AreaDto> {

    private final boolean showButtons;

    public AreaGridComponent(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<AreaDto> itemsAction) {
        super(deleteAction, appEnv, itemsAction);
        showButtons = true;
        initGrid();
    }

    public AreaGridComponent(AppEnv appEnv, ItemsAction<AreaDto> itemsAction) {
        super(appEnv, itemsAction);
        showButtons = false;
        initGrid();
    }

    private void initGrid() {
        addColumn(AreaDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(AreaDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");

        if (showButtons) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

            addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
        }
    }

    private HorizontalLayout getGridButtons(AreaDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit area",
                "Are you sure you want to delete area {0} ?", "Delete area ");
        AppGridDataVariables dataVariables = new AppGridDataVariables(Permission.AREA_EDIT, Permission.AREA_DELETE,
                clickedItem.getId(), "", AreaDetailView.ROUTE, clickedItem.getDeleted());
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(AreaDetailView.ROUTE + "/" + code);
    }
}
