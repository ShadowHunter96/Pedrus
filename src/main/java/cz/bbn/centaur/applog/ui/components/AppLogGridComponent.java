package cz.bbn.cerberus.applog.ui.components;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.applog.dto.AppLogDto;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.BasicInfoDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.translation.Transl;

public class AppLogGridComponent extends AppInfiniteGrid<AppLogDto> {

    public AppLogGridComponent(AppEnv appEnv, ItemsAction<AppLogDto> itemsAction) {
        super(appEnv, itemsAction);
        initGrid();
    }

    private void initGrid() {
        addColumn(item -> AppUtils.formatDateTime(item.getDate(), true)).setHeader(Transl.get("Date"))
                .setSortable(true).setKey("date");
        addColumn(AppLogDto::getAction).setHeader(Transl.get("Action")).setSortable(true).setKey("action");
        addColumn(this::getUserName).setHeader(Transl.get("User")).setSortable(true).setKey("userId");
        addColumn(AppLogDto::getMessage).setHeader(Transl.get("Message")).setSortable(true).setKey("message");
        addColumn(AppLogDto::getAppId).setHeader(Transl.get("Id")).setSortable(true).setKey("appId");

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_SMALL)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> showDialog(event.getItem()));
        this.setSizeFull();
    }

    private String getUserName(AppLogDto appLogDto) {
        if (appLogDto.getUserDto() == null) {
            return "";
        } else {
            return appLogDto.getUserDto().getName();
        }
    }

    private HorizontalLayout getGridButtons(AppLogDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        Button button = VaadinComponents.getViewButton();
        button.addClickListener(buttonClickEvent -> showDialog(clickedItem));
        buttons.add(button);
        return buttons;
    }

    private void showDialog(AppLogDto clickedItem) {
        TextArea message = new TextArea(Transl.get("Message"));
        message.setValue(clickedItem.getMessage());
        message.setWidth("30em");
        BasicInfoDialog basicInfoDialog = new BasicInfoDialog(message, Transl.get("Message detail"));
        basicInfoDialog.open();
    }

}
