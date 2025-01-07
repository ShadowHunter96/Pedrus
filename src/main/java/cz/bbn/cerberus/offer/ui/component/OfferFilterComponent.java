package cz.bbn.cerberus.offer.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.field.AppMoneyField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.offer.dto.OfferFilterDto;
import cz.bbn.cerberus.offer.dto.OfferState;
import cz.bbn.cerberus.offer.ui.OfferView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class OfferFilterComponent extends FormLayout {

    private final Button search;
    private final String objectId;
    private final ObjectType objectType;
    private final boolean showListFilter;
    private final String params;
    private OfferGridComponent grid;

    private DatePicker validityDateFrom;
    private DatePicker validityDateTo;

    private TextField nameOrId;
    private MultiSelectComboBox<Boolean> sent;
    private MultiSelectComboBox<OfferState> offerState;
    private ListService listService;

    private MultiSelectComboBox<SubjectDto> customer;
    private AppMoneyField priceFrom;
    private AppMoneyField priceTo;

    private Checkbox showDeleted;
    private Checkbox onlyEditPermission;

    private HistoryBreadcrumbs historyBreadcrumbs;

    public OfferFilterComponent(Button search, String objectId, ObjectType objectType) {
        this.search = search;
        this.objectId = objectId;
        this.objectType = objectType;
        this.showListFilter = false;
        this.params = null;
        initComponent();
    }

    public OfferFilterComponent(Button search, ListService listService, String params,
                                HistoryBreadcrumbs historyBreadcrumbs) {
        this.search = search;
        this.objectId = null;
        this.objectType = null;
        this.showListFilter = true;
        this.listService = listService;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        initComponent();
    }

    private void initComponent() {
        nameOrId = new TextField(Transl.get("Name/Id"));
        this.add(nameOrId);

        sent = new MultiSelectComboBox<>(Transl.get("Sent"));
        customer = new MultiSelectComboBox<>(Transl.get("Customer"));
        priceFrom = new AppMoneyField(Transl.get("Price from"));
        priceTo = new AppMoneyField(Transl.get("Price to"));

        if (!showListFilter) {
            sent.setItems(Boolean.TRUE, Boolean.FALSE);
            sent.setItemLabelGenerator(aBoolean -> Boolean.TRUE.equals(aBoolean) ? Transl.get("Yes") : Transl.get("No"));
            this.add(sent);
        } else {
            customer.setItems(listService.getSubjectDtoListByCustomer());
            customer.setItemLabelGenerator(SubjectDto::getName);
            this.add(customer);
            this.add(priceFrom);
            this.add(priceTo);
        }

        offerState = new MultiSelectComboBox<>(Transl.get("State"));
        offerState.setItems(OfferState.values());
        offerState.setItemLabelGenerator(offerState1 -> Transl.get(offerState1.name()));
        this.add(offerState);

        validityDateFrom = VaadinComponents.getDatePicker(Transl.get("Offer validity from"), null);
        this.add(validityDateFrom);

        validityDateTo = VaadinComponents.getDatePicker(Transl.get("Offer validity to"), null);
        this.add(validityDateTo);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        showDeleted.setValue(false);
        if (SecurityUtils.hasPermission(Permission.OFFER_SHOW_DELETED)) {
            this.add(showDeleted);
        }
        onlyEditPermission = new Checkbox(Transl.get("Only editable"));
        onlyEditPermission.addValueChangeListener(event -> {
            grid.getPriceWithoutVat().setVisible(event.getValue());
        });
        this.add(onlyEditPermission);

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("nameOrId")) {
            nameOrId.setValue(map.get("nameOrId"));
        }
        if (map.containsKey("sent")) {
            String[] sentStrList = map.get("sent").split(",");
            List<Boolean> sentList = new ArrayList<>();
            for (String sentStr : sentStrList) {
                if ("true".equalsIgnoreCase(map.get("sent")) || "false".equalsIgnoreCase(map.get("sent"))) {
                    sentList.add(Boolean.valueOf(sentStr));
                }
            }
            sent.setValue(sentList);
        }
        if (map.containsKey("customer")) {
            String[] subjectIdList = map.get("customer").split(",");
            List<SubjectDto> subjectList = new ArrayList<>();
            Map<String, SubjectDto> subjectMap = listService.getSubjectMap();
            for (String subjectId : subjectIdList) {
                if (SecurityUtils.hasCustomPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue(),
                        subjectId, Permission.SUPPLIER_VIEW.name()) && subjectMap.containsKey(subjectId)) {
                    subjectList.add(subjectMap.get(subjectId));
                }
            }
            customer.setValue(subjectList);
        }
        if (map.containsKey("priceFrom")) {
            priceFrom.setValue(BigDecimal.valueOf(Double.parseDouble(map.get("priceFrom"))));
        }
        if (map.containsKey("priceTo")) {
            priceTo.setValue(BigDecimal.valueOf(Double.parseDouble(map.get("priceTo"))));
        }
        if (map.containsKey("offerState")) {
            String[] actualState = map.get("offerState").split(",");
            Set<OfferState> offerStateSet = new HashSet<>();
            for (String stateStr : actualState) {
                if (OfferState.stateExist(stateStr)) {
                    offerStateSet.add(OfferState.valueOf(stateStr));
                }
            }
            offerState.setValue(offerStateSet);
        }
        if (map.containsKey("validityDateFrom")) {
            validityDateFrom.setValue(LocalDate.parse(map.get("validityDateFrom")));
        }
        if (map.containsKey("validityDateTo")) {
            validityDateTo.setValue(LocalDate.parse(map.get("validityDateTo")));
        }
        if (map.containsKey("onlyEditPermission")) {
            onlyEditPermission.setValue("true".equalsIgnoreCase(map.get("onlyEditPermission")));
            grid.getPriceWithoutVat().setVisible(onlyEditPermission.getValue());
        } else {
            onlyEditPermission.setValue(true);
            grid.getPriceWithoutVat().setVisible(true);
        }
        if (map.containsKey("showDeleted") && SecurityUtils.hasPermission(Permission.OFFER_SHOW_DELETED)) {
            showDeleted.setValue("true".equalsIgnoreCase(map.get("showDeleted")));
        }
    }

    public void fillUrl() {
        String paramUrl = OfferView.ROUTE.concat("/");

        if (nameOrId.getValue() != null && !nameOrId.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&nameOrId=").concat(nameOrId.getValue());
        }
        if (sent.getValue() != null && !sent.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&sent=");
            List<String> sentStrList = new ArrayList<>();
            for (Boolean value : sent.getValue()) {
                sentStrList.add(value.toString());
            }
            paramUrl = paramUrl.concat(String.join(",", sentStrList));
        }
        if (customer.getValue() != null && !customer.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&customer=");
            List<String> subjectIdList = new ArrayList<>();
            for (SubjectDto subjectDto : customer.getValue()) {
                subjectIdList.add(subjectDto.getId());
            }
            paramUrl = paramUrl.concat(String.join(",", subjectIdList));
        }
        if (priceFrom.getValue() != null) {
            paramUrl = paramUrl.concat("&priceFrom=").concat(priceFrom.getValue().toString());
        }
        if (priceTo.getValue() != null) {
            paramUrl = paramUrl.concat("&priceTo=").concat(priceTo.getValue().toString());
        }
        if (offerState.getValue() != null && !offerState.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&offerState=");
            List<String> stateStrList = new ArrayList<>();
            for (OfferState stateEn : offerState.getValue()) {
                stateStrList.add(stateEn.name());
            }
            paramUrl = paramUrl.concat(String.join(",", stateStrList));
        }
        if (validityDateFrom.getValue() != null) {
            paramUrl = paramUrl.concat("&validityDateFrom=").concat(validityDateFrom.getValue().toString());
        }
        if (validityDateTo.getValue() != null) {
            paramUrl = paramUrl.concat("&validityDateTo=").concat(validityDateTo.getValue().toString());
        }
        if (onlyEditPermission.getValue() != null) {
            paramUrl = paramUrl.concat("&onlyEditPermission=").concat(onlyEditPermission.getValue().toString());
        }
        if (showDeleted.getValue() != null && SecurityUtils.hasPermission(Permission.OFFER_SHOW_DELETED)) {
            paramUrl = paramUrl.concat("&showDeleted=").concat(showDeleted.getValue().toString());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public OfferFilterDto getOfferFilterDto() {
        OfferFilterDto dto = new OfferFilterDto();
        dto.setNameOrId(nameOrId.getValue());
        dto.setObjectType(objectType);
        dto.setObjectId(objectId);
        dto.setSent(sent.getSelectedItems());
        dto.setOfferStateSet(offerState.getSelectedItems());
        dto.setCustomerSet(customer.getSelectedItems());
        dto.setPriceFrom(priceFrom.getValue());
        dto.setPriceTo(priceTo.getValue());
        dto.setValidityDateFrom(validityDateFrom.getValue());
        dto.setValidityDateTo(validityDateTo.getValue());
        dto.setShowDeleted(showDeleted.getValue());
        dto.setOnlyEditPermission(onlyEditPermission.getValue());
        return dto;
    }

    public void setGrid(OfferGridComponent grid) {
        this.grid = grid;
    }

    public OfferGridComponent getGrid() {
        return grid;
    }
}
