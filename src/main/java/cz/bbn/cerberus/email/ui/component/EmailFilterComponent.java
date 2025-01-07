package cz.bbn.cerberus.email.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datetimepicker.DateTimePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.email.dto.EmailFilterDto;
import cz.bbn.cerberus.email.dto.SimpleItemDto;
import cz.bbn.cerberus.email.ui.EmailView;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Map;

public class EmailFilterComponent extends FormLayout {

    private final Button searchButton;
    private final Binder<EmailFilterDto> binder;
    private final EmailComponentOperations componentOperations;
    private final DomainEnum entityType;
    private final String entityId;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;

    private TextField subject;
    private TextField sender;
    private DateTimePicker sentFrom;
    private DateTimePicker sentTo;
    private IntegerField noOfAttachments;
    private ComboBox<DomainEnum> entityTypeCB;
    private ComboBox<SubjectDto> customer;
    private ComboBox<SimpleItemDto> entityName;
    private TextField searchAll;

    public EmailFilterComponent(Button searchButton, EmailComponentOperations componentOperations,
                                String params, HistoryBreadcrumbs historyBreadcrumbs) {
        this.searchButton = searchButton;
        this.binder = new Binder<>();
        this.componentOperations = componentOperations;
        this.entityId = null;
        this.entityType = null;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        initFilter();
        fillFilterFromUrl();
    }

    public EmailFilterComponent(Button searchButton, EmailComponentOperations componentOperations,
                                DomainEnum entityType, String entityId) {
        this.searchButton = searchButton;
        this.entityType = entityType;
        this.entityId = entityId;
        this.binder = new Binder<>();
        this.componentOperations = componentOperations;
        this.params = null;
        this.historyBreadcrumbs = null;
        initFilter();
    }

    private void initFilter() {
        binder.setBean(new EmailFilterDto());

        subject = new TextField(Transl.get("Email subject"));
        binder.forField(subject).bind(EmailFilterDto::getSubject, EmailFilterDto::setSubject);
        add(subject);

        sender = new TextField(Transl.get("Sender"));
        binder.forField(sender).bind(EmailFilterDto::getSender, EmailFilterDto::setSender);
        add(sender);

        sentFrom = VaadinComponents.getDateTimePicker(Transl.get("Sent from"), null);
        binder.forField(sentFrom).bind(EmailFilterDto::getFromDateTime, EmailFilterDto::setFromDateTime);
        add(sentFrom);

        sentTo = VaadinComponents.getDateTimePicker(Transl.get("Sent to"), null);
        binder.forField(sentTo).bind(EmailFilterDto::getToDateTime, EmailFilterDto::setToDateTime);
        add(sentTo);

        sentFrom.addValueChangeListener(e -> sentTo.setMin(e.getValue()));
        sentTo.addValueChangeListener(e -> sentFrom.setMax(e.getValue()));

        noOfAttachments = new IntegerField(Transl.get("No. of attachments"));
        binder.forField(noOfAttachments).bind(EmailFilterDto::getNoOfAttachments, EmailFilterDto::setNoOfAttachments);
        add(noOfAttachments);

        if (entityId == null) {
            customer = new ComboBox<>(Transl.get("Customer"));
            customer.setItems(componentOperations.getCustomerList());
            customer.setItemLabelGenerator(SubjectDto::getName);
            binder.forField(customer).bind(EmailFilterDto::getCustomer, EmailFilterDto::setCustomer);
            add(customer);

            entityTypeCB = new ComboBox<>(Transl.get("Entity type"));
            entityTypeCB.setItems(componentOperations.getSubjectDomainEnum());
            entityTypeCB.setItemLabelGenerator(DomainEnum::getTranslatedName);
            binder.forField(entityTypeCB).bind(EmailFilterDto::getEntityType, EmailFilterDto::setEntityType);
            add(entityTypeCB);

            entityName = new ComboBox<>(Transl.get("Entity name"));
            entityName.setReadOnly(true);
            entityName.setItemLabelGenerator(SimpleItemDto::getName);
            binder.forField(entityName).bind(EmailFilterDto::getItem, EmailFilterDto::setItem);
            add(entityName);

            customer.addValueChangeListener(e -> {
                entityName.setValue(null);
                if (e.getValue() != null && entityTypeCB.getValue() != null
                        && entityTypeCB.getValue() != DomainEnum.SUBJECT_DOMAIN_NAME) {
                    entityName.setItems(
                            componentOperations.getSimpleItems(entityTypeCB.getValue(), e.getValue().getId()));
                    entityName.setReadOnly(false);
                } else {
                    entityName.setItems(new ArrayList<>());
                    entityName.setReadOnly(true);
                }
            });

            searchAll = new TextField(Transl.get("Search"));
            binder.forField(searchAll).bind(EmailFilterDto::getSearchAll, EmailFilterDto::setSearchAll);
            add(searchAll);

            entityTypeCB.addValueChangeListener(e -> {
                entityName.setValue(null);
                if (e.getValue() != null && customer.getValue() != null
                        && e.getValue() != DomainEnum.SUBJECT_DOMAIN_NAME) {
                    entityName.setItems(
                            componentOperations.getSimpleItems(e.getValue(), customer.getValue().getId()));
                    entityName.setReadOnly(false);
                } else {
                    entityName.setItems(new ArrayList<>());
                    entityName.setReadOnly(true);
                }
            });
        } else {
            binder.getBean().setEntityType(entityType);
            binder.getBean().setEntityId(entityId);
        }

        add(searchButton);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("emailSubject")) {
            subject.setValue(map.get("emailSubject"));
        }
        if (map.containsKey("sender")) {
            sender.setValue(map.get("sender"));
        }
        if (map.containsKey("sentFrom")) {
            sentFrom.setValue(LocalDateTime.parse(map.get("sentFrom")));
        }
        if (map.containsKey("sentTo")) {
            sentTo.setValue(LocalDateTime.parse(map.get("sentTo")));
        }
        if (map.containsKey("noOfAttachments")) {
            noOfAttachments.setValue(Integer.valueOf(map.get("noOfAttachments")));
        }
        if (map.containsKey("customer")) {
            for (SubjectDto subjectDto : componentOperations.getCustomerList()) {
                if (subjectDto.getId().equals(map.get("customer"))) {
                    customer.setValue(subjectDto);
                }
            }
        }
        if (map.containsKey("entityType")) {
            for (DomainEnum domainEnum : componentOperations.getSubjectDomainEnum()) {
                if (domainEnum.getValue().equals(map.get("entityType"))) {
                    entityTypeCB.setValue(domainEnum);
                }
            }
        }
        if (map.containsKey("entityId") && map.containsKey("entityType") && map.containsKey("customer")
                && !map.get("entityType").equals(DomainEnum.SUBJECT_DOMAIN_NAME.getValue())) {
            for (SimpleItemDto simpleItemDto : componentOperations.getSimpleItems(
                    DomainEnum.getDomainByValue(map.get("entityType")), map.get("customer"))) {
                if (simpleItemDto.getId().equals(map.get("entityId"))) {
                    entityName.setValue(simpleItemDto);
                }
            }
        }
        if (map.containsKey("searchAll")) {
            searchAll.setValue(map.get("searchAll"));
        }
    }

    public void fillUrl() {
        String paramUrl = EmailView.ROUTE.concat("/");
        if (subject.getValue() != null && !subject.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&emailSubject=").concat(subject.getValue());
        }
        if (sender.getValue() != null && !sender.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&sender=").concat(sender.getValue());
        }
        if (sentFrom.getValue() != null) {
            paramUrl = paramUrl.concat("&sentFrom=").concat(sentFrom.getValue().toString());
        }
        if (sentTo.getValue() != null) {
            paramUrl = paramUrl.concat("&sentTo=").concat(sentTo.getValue().toString());
        }
        if (noOfAttachments.getValue() != null) {
            paramUrl = paramUrl.concat("&noOfAttachments=").concat(noOfAttachments.getValue().toString());
        }
        if (customer.getValue() != null) {
            paramUrl = paramUrl.concat("&customer=").concat(customer.getValue().getId());
        }
        if (entityTypeCB.getValue() != null) {
            paramUrl = paramUrl.concat("&entityType=").concat(entityTypeCB.getValue().getValue());
        }
        if (entityName.getValue() != null) {
            paramUrl = paramUrl.concat("&entityId=").concat(entityName.getValue().getId());
        }
        if (searchAll.getValue() != null && !searchAll.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&searchAll=").concat(searchAll.getValue());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public EmailFilterDto getEmailFilterDto() {
        return binder.getBean();
    }
}
