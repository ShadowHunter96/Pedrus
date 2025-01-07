package cz.bbn.cerberus.dsmessage.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dsmessage.dto.DsMessageFilterDto;
import cz.bbn.cerberus.dsmessage.persistance.DsMessageType;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class DsMessageFilterComponent extends FormLayout {

    private final Button search;
    private final List<String> recipientList;
    private final List<String> senderNameList;

    private Checkbox showDeleted;
    private DatePicker deliveryTimeFrom;
    private DatePicker deliveryTimeTo;
    private DatePicker createdInAppFrom;
    private DatePicker createdInAppTo;
    private ComboBox<String> recipientId;
    private ComboBox<String> senderName;
    private Checkbox viewed;
    private ComboBox<DsMessageType> type;

    public DsMessageFilterComponent(Button search, List<String> recipientList, List<String> senderNameList) {
        this.search = search;
        this.recipientList = recipientList;
        this.senderNameList = senderNameList;
        initComponent();
    }

    private void initComponent(){
        deliveryTimeFrom = VaadinComponents.getDatePicker(Transl.get("Delivery time from"), null);
        this.add(deliveryTimeFrom);

        deliveryTimeTo = VaadinComponents.getDatePicker(Transl.get("Delivery time to"),null);
        this.add(deliveryTimeTo);

        createdInAppFrom = VaadinComponents.getDatePicker(Transl.get("Created in app from"),null);
        this.add(createdInAppFrom);

        createdInAppTo = VaadinComponents.getDatePicker(Transl.get("Created in app to"),null);
        this.add(createdInAppTo);

        recipientList.add(0, "");
        recipientId = new ComboBox<>(Transl.get("Recipient id"));
        recipientId.setItems(recipientList);
        this.add(recipientId);

        senderNameList.add(0, "");
        senderName = new ComboBox<>(Transl.get("Sender name"));
        senderName.setItems(senderNameList);
        this.add(senderName);

        type = new ComboBox<>(Transl.get("Type"));
        type.setItems(DsMessageType.values());
        type.setItemLabelGenerator(dsMessageType -> Transl.get(dsMessageType.name()));
        type.setValue(DsMessageType.ALL);
        this.add(type);

        viewed = new Checkbox(Transl.get("Viewed"));
        this.add(viewed);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if(SecurityUtils.hasPermission(Permission.DS_MESSAGE_SHOW_DELETED)) {
            this.add(showDeleted);
        }

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public DsMessageFilterDto getDsMessageFilterDto(){
        DsMessageFilterDto dsMessageFilterDto = new DsMessageFilterDto();
        if(deliveryTimeFrom.getValue() != null){
            dsMessageFilterDto.setDeliveryTimeFrom(deliveryTimeFrom.getValue().atTime(0, 0));
        }
        if(deliveryTimeTo.getValue() != null){
            dsMessageFilterDto.setDeliveryTimeTo(deliveryTimeTo.getValue().atTime(12, 59));
        }
        if(createdInAppFrom.getValue() != null){
            dsMessageFilterDto.setDeliveryTimeFrom(createdInAppFrom.getValue().atTime(0, 0));
        }
        if(createdInAppTo.getValue() != null){
            dsMessageFilterDto.setDeliveryTimeTo(createdInAppTo.getValue().atTime(12, 59));
        }
        dsMessageFilterDto.setRecipientId(recipientId.getValue());
        dsMessageFilterDto.setSenderName(senderName.getValue());
        dsMessageFilterDto.setViewed(viewed.getValue());
        dsMessageFilterDto.setShowDeleted(showDeleted.getValue());
        dsMessageFilterDto.setType(type.getValue());
        return dsMessageFilterDto;
    }
}
