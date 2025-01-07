package cz.bbn.cerberus.dsmessage.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.dsmessage.dto.DsMessageSimpleDto;
import cz.bbn.cerberus.dsmessage.ui.DsMessageDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class DsMessageGridComponent extends AppInfiniteGrid<DsMessageSimpleDto> {

    public DsMessageGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                  ItemsAction<DsMessageSimpleDto> itemsAction) {
        super(deleteAction, appEnv, itemsAction);
        initComponent();
    }

    private void initComponent() {
        setSizeFull();
        addColumn(item -> AppUtils.formatDateTime(item.getDeliveryTime(), true)).setHeader(Transl.get("Delivery time"))
                .setSortable(true).setKey("deliveryTime");
        addColumn(DsMessageSimpleDto::getId).setHeader(Transl.get("DS id")).setSortable(true).setKey("id");
        addColumn(DsMessageSimpleDto::getRecipientId).setHeader(Transl.get("Recipient id"))
                .setSortable(true).setKey("recipientId");
        addColumn(DsMessageSimpleDto::getSenderName).setHeader(Transl.get("Sender name"))
                .setSortable(true).setKey("senderName");
        addColumn(DsMessageSimpleDto::getSubject).setHeader(Transl.get("Subject")).setSortable(true).setKey("subject");
        addColumn(DsMessageSimpleDto::getAttachementSize).setHeader(Transl.get("Attachement size"))
                .setSortable(true).setKey("attachementSize");
        addColumn(item -> AppUtils.formatDateTime(item.getCreatedInAppTime(), true))
                .setHeader(Transl.get("Created in app")).setSortable(true).setKey("createdInApp");
        addColumn(dsMessageSimpleDto -> Transl.get(dsMessageSimpleDto.getType().name())).setHeader(Transl.get("type"))
                .setSortable(true).setKey("type");
        addColumn(new ComponentRenderer<>(this::getViewedIcon)).setHeader(Transl.get("Viewed"))
                .setSortable(true).setKey("viewed");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }

    private Span getViewedIcon(DsMessageSimpleDto dto) {
        return new Span(VaadinComponents.getBooleanIcon(dto.getViewed()));
    }


    private HorizontalLayout getGridButtons(DsMessageSimpleDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit ds setting",
                "Are you sure you want to delete ds {0} ?", "Delete ds");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                null, Permission.DS_MESSAGE_DELETE, String.valueOf(clickedItem.getId()), "",
                DsMessageDetailView.ROUTE, clickedItem.getDeleted());
        return getSimpleGridButtons(dataVariables, stringVariables, this, clickedItem.getDeleted());
    }

    private void gridClicked(Long id) {
        UI.getCurrent().navigate(DsMessageDetailView.ROUTE + "/" + id);
    }
}
