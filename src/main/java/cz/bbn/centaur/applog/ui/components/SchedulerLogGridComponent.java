package cz.bbn.cerberus.applog.ui.components;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.BasicInfoDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.schedulerlog.dto.SchedulerLogDto;
import cz.bbn.cerberus.translation.Transl;


public class SchedulerLogGridComponent extends AppInfiniteGrid<SchedulerLogDto> {

    public SchedulerLogGridComponent(AppEnv appEnv, ItemsAction<SchedulerLogDto> itemsAction) {
        super(appEnv, itemsAction);
        initGrid();
    }

    private void initGrid() {
        addColumn(schedulerLogDto -> AppUtils.formatDateTime(schedulerLogDto.getDate(), true))
                .setHeader(Transl.get("Date")).setSortable(true).setKey("date");
        addColumn(SchedulerLogDto::getDescription).setHeader(Transl.get("Description")).setSortable(true)
                .setKey("description");

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_SMALL)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> showDialog(event.getItem()));

        this.setSizeFull();
    }

    private HorizontalLayout getGridButtons(SchedulerLogDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");
        Button show = VaadinComponents.getViewButton();
        show.addClickListener(buttonClickEvent ->
                showDialog(clickedItem)
        );
        buttons.add(show);
        return buttons;
    }

    private void showDialog(SchedulerLogDto clickedItem) {
        TextArea exception = new TextArea(Transl.get("Exception"));
        exception.setValue(clickedItem.getException());
        exception.setWidth("50em");
        BasicInfoDialog basicInfoDialog = new BasicInfoDialog(exception, Transl.get("Exception detail"));
        basicInfoDialog.open();
    }
}
