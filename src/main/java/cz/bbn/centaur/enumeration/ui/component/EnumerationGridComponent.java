package cz.bbn.cerberus.enumeration.ui.component;

import com.vaadin.flow.component.UI;
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
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.enumeration.ui.EnumerationDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;


public class EnumerationGridComponent extends AppInfiniteGrid<EnumerationDto> {

    private final boolean showButtons;
    private final int tabIndex;

    protected EnumerationGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                       ItemsAction<EnumerationDto> itemsAction,
                                       boolean showButtons, int tabIndex) {
        super(deleteAction, appEnv, itemsAction);
        this.showButtons = showButtons;
        this.tabIndex = tabIndex;
        initGrid();
    }

    private void initGrid() {
        addColumn(EnumerationDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(EnumerationDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(contactPersonTypeDto ->
                VaadinComponents.getCheckUncheckLayoutNullTrue(contactPersonTypeDto.getAllowed())))
                .setHeader(Transl.get("Allowed")).setSortable(true).setKey("allowed")
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0);
        if (showButtons) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

            addItemDoubleClickListener(event -> gridClicked(String.valueOf(event.getItem().getId())));
        }
    }

    private HorizontalLayout getGridButtons(EnumerationDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.valueOf(
                clickedItem.getEnumerationTypeDto().getPermissionKey().concat("_DELETE")))) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, String.valueOf(clickedItem.getId()));
            delete.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getId()),
                                Transl.get("Are you sure you want to delete "
                                                .concat(clickedItem.getEnumerationTypeDto().getTranslationKey())
                                                .concat("{0} ?")
                                        , clickedItem.getName()),
                                getAppEnv(), true);
                deleteConfirmDialog.open();

            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete ")
                    .concat(clickedItem.getEnumerationTypeDto().getTranslationKey()));
            buttons.add(delete);
        }

        return buttons;
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(EnumerationDetailView.ROUTE + "/" + code + "-" + tabIndex);
    }
}
