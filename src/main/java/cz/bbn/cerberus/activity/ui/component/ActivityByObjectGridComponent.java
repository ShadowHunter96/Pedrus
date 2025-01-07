package cz.bbn.cerberus.activity.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.activity.dto.ActivityByObjectDto;
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
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class ActivityByObjectGridComponent extends AppInfiniteGrid<ActivityByObjectDto> {

    public ActivityByObjectGridComponent(
            DeleteAction deleteAction, AppEnv appEnv, ItemsAction<ActivityByObjectDto> itemsAction) {
        super(deleteAction, appEnv, itemsAction);
        initGrid();
    }

    private void initGrid() {
        addColumn(activityByObjectDto -> activityByObjectDto.getEnumerationDto().getName())
                .setHeader(Transl.get("Id")).setSortable(true).setKey("id.activityId");
        addColumn(activityByObjectDto -> activityByObjectDto.getEnumerationDto()
                .getName()).setHeader(Transl.get("Name")).setSortable(true).setKey("enumerationEntity.name");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_SMALL)
                .setFlexGrow(1).setTextAlign(ColumnTextAlign.CENTER);
    }

    private HorizontalLayout getGridButtons(ActivityByObjectDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.ACTIVITY_BY_OBJECT_UNLINK)) {
            Button unlink = VaadinComponents.getUnlinkButton();
            AppUtils.addRfClassToGridButton(unlink, String.valueOf(clickedItem.getId()));
            unlink.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getId().getActivityId()),
                                Transl.get("Are you sure you want to unlink activity {0} ?",
                                        String.valueOf(clickedItem.getEnumerationDto().getName())), getAppEnv(), true);
                deleteConfirmDialog.open();
            });
            unlink.getElement().setProperty(TextValues.TITLE, Transl.get("Unclink activity"));
            buttons.add(unlink);
        }

        return buttons;
    }
}
