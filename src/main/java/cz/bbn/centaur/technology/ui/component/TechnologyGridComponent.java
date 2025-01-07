package cz.bbn.cerberus.technology.ui.component;

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
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.technology.ui.TechnologyDetailView;
import cz.bbn.cerberus.translation.Transl;

public class TechnologyGridComponent extends AppInfiniteGrid<TechnologyDto> {

    private final boolean showButtons;

    public TechnologyGridComponent(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<TechnologyDto> itemsAction) {
        super(deleteAction, appEnv, itemsAction);
        showButtons = true;
        initGrid();
    }

    private void initGrid() {
        addColumn(TechnologyDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(TechnologyDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        if (showButtons) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

            addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
        }
    }

    private HorizontalLayout getGridButtons(TechnologyDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit technology",
                "Are you sure you want to delete technology {0} ?", "Delete technology");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.TECHNOLOGY_EDIT, Permission.TECHNOLOGY_DELETE, clickedItem.getId(), "",
                TechnologyDetailView.ROUTE, clickedItem.getDeleted());
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(TechnologyDetailView.ROUTE + "/" + code);
    }
}
