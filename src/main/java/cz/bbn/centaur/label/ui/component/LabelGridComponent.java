package cz.bbn.cerberus.label.ui.component;

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
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.ui.LabelDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class LabelGridComponent extends AppInfiniteGrid<LabelDto> {

    private final DeleteAction deleteAction;
    private final ListAction<String> listAction;

    public LabelGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                              ItemsAction<LabelDto> itemsAction, ListAction<String> listAction) {
        super(deleteAction, appEnv, itemsAction);
        this.deleteAction = deleteAction;
        this.listAction = listAction;
        initGrid();
    }

    public LabelGridComponent(AppEnv appEnv, ItemsAction<LabelDto> itemsAction) {
        super(appEnv, itemsAction);
        deleteAction = null;
        this.listAction = null;
        initGrid();
    }

    private void initGrid() {
        addColumn(LabelDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(LabelDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        if (deleteAction != null) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

            addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
        }
    }

    private HorizontalLayout getGridButtons(LabelDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.LABEL_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, clickedItem.getId());
            delete.addClickListener(buttonClickEvent -> {
                List<String> subjectList = listAction.getList(clickedItem.getId());
                if (subjectList.isEmpty()) {
                    DeleteConfirmDialog dialog =
                            new DeleteConfirmDialog(this, clickedItem.getId(), Transl.get(
                                    "Are you sure you want to delete label {0} ?",
                                    clickedItem.getId()), getAppEnv(), true);
                    dialog.open();
                } else {
                    LabelDeleteWarningDialog dialog = new LabelDeleteWarningDialog(clickedItem.getId(),
                            listAction.getList(clickedItem.getId()), deleteAction, this, getAppEnv());
                    dialog.open();
                }
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete label"));
            buttons.add(delete);
        }

        return buttons;
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(LabelDetailView.ROUTE + "/" + code);
    }
}
