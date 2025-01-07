package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.enums.FilterBoolean;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.invoice.dto.InvoiceFilterDto;
import cz.bbn.cerberus.invoice.dto.InvoiceState;
import cz.bbn.cerberus.invoice.ui.InvoicingView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

public class InvoiceFilterComponent extends FormLayout {

    private final List<UserDto> userList;
    private final List<ContractDto> contractDtoList;
    private final List<SubjectDto> subjectDtoList;
    private final Button search;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;

    private DatePicker invoicingDateStart;
    private DatePicker invoicingDateEnd;
    private DatePicker issueDateStart;
    private DatePicker issueDateEnd;
    private ComboBox<InvoiceState> invoiceState;
    private ComboBox<FilterBoolean> payed;
    private ComboBox<ContractDto> contractId;
    private ComboBox<UserDto> userDto;
    private ComboBox<SubjectDto> subjectId;
    private Checkbox showDeleted;
    private Checkbox onlyEditPermission;
    private ComboBox<FilterBoolean> transferProtocol;

    public InvoiceFilterComponent(List<UserDto> userList, List<ContractDto> contractDtoList,
                                  List<SubjectDto> subjectDtoList, Button search, String params,
                                  HistoryBreadcrumbs historyBreadcrumbs) {
        this.userList = userList;
        this.contractDtoList = contractDtoList;
        this.subjectDtoList = subjectDtoList;
        this.search = search;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        initComponents();
    }

    private void initComponents() {
        payed = VaadinComponents.getFilterBooleanComboBox(Transl.get("Payed"));
        this.add(payed);

        invoicingDateStart = VaadinComponents.getDatePicker(null);
        invoicingDateStart.setLabel(Transl.get("Invoicing date from"));

        invoicingDateEnd = VaadinComponents.getDatePicker(null);
        invoicingDateEnd.setLabel(Transl.get("Invoicing date to"));

        invoicingDateStart.setMax(invoicingDateEnd.getValue());
        invoicingDateEnd.setMin(invoicingDateStart.getValue());

        invoicingDateEnd.addValueChangeListener(e -> invoicingDateStart.setMax(e.getValue()));
        invoicingDateStart.addValueChangeListener(e -> invoicingDateEnd.setMin(e.getValue()));

        this.add(invoicingDateStart, invoicingDateEnd);

        userDto = new ComboBox<>(Transl.get("Owner"));
        userDto.setItemLabelGenerator(UserDto::getName);
        userList.add(0, new UserDto(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE)));
        userDto.setItems(userList);
        userDto.setValue(new UserDto(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE)));
        add(userDto);

        issueDateStart = VaadinComponents.getDatePicker(null);
        issueDateStart.setLabel(Transl.get("Issue date start"));

        issueDateEnd = VaadinComponents.getDatePicker(null);
        issueDateEnd.setLabel(Transl.get("Issue date end"));

        issueDateStart.setMax(invoicingDateEnd.getValue());
        issueDateEnd.setMin(invoicingDateStart.getValue());

        issueDateStart.addValueChangeListener(e -> issueDateEnd.setMin(e.getValue()));
        issueDateEnd.addValueChangeListener(e -> issueDateStart.setMax(e.getValue()));

        this.add(issueDateStart, issueDateEnd);

        invoiceState = new ComboBox<>(Transl.get("State"));
        invoiceState.setItems(InvoiceState.values());
        invoiceState.setItemLabelGenerator(actualInvoiceState -> Transl.get(actualInvoiceState.name()));
        this.add(invoiceState);

        contractId = new ComboBox<>(Transl.get("Contract"));
        ContractDto contractDto = new ContractDto();
        contractDto.setName(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE));
        contractDtoList.add(0, contractDto);
        contractId.setItems(contractDtoList);
        contractId.setValue(contractDto);
        contractId.setItemLabelGenerator(ContractDto::getName);
        this.add(contractId);

        subjectId = new ComboBox<>(Transl.get("Subject"));
        SubjectDto subjectDto = new SubjectDto();
        subjectDto.setName(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE));
        subjectDtoList.add(0, subjectDto);
        subjectId.setItems(subjectDtoList);
        subjectId.setValue(subjectDto);
        subjectId.setItemLabelGenerator(SubjectDto::getName);
        this.add(subjectId);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_SHOW_DELETED)) {
            this.add(showDeleted);
        }

        onlyEditPermission = new Checkbox(Transl.get("Only editable"));
        onlyEditPermission.setValue(true);
        this.add(onlyEditPermission);

        transferProtocol = VaadinComponents.getFilterBooleanComboBox(Transl.get("Transfer protocol"));
        this.add(transferProtocol);

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
        fillFilterFromUrl();
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("payed")) {
            payed.setValue(FilterBoolean.getValueForFilter(map.get("payed")));
        }
        if (map.containsKey("invoicingDateStart")) {
            invoicingDateStart.setValue(LocalDate.parse(map.get("invoicingDateStart")));
        }
        if (map.containsKey("invoicingDateEnd")) {
            invoicingDateEnd.setValue(LocalDate.parse(map.get("invoicingDateEnd")));
        }
        if (map.containsKey("user")) {
            for (UserDto user : userList) {
                if (user.getId() != null && String.valueOf(user.getId()).equals(map.get("user"))) {
                    userDto.setValue(user);
                }
            }
        }
        if (map.containsKey("issueDateStart")) {
            issueDateStart.setValue(LocalDate.parse(map.get("issueDateStart")));
        }
        if (map.containsKey("issueDateEnd")) {
            issueDateEnd.setValue(LocalDate.parse(map.get("issueDateEnd")));
        }
        if (map.containsKey("invoiceState")) {
            invoiceState.setValue(InvoiceState.valueOf(map.get("invoiceState")));
        }
        if (map.containsKey("contractId")) {
            for (ContractDto contractDto : contractDtoList) {
                if (contractDto.getId() != null && contractDto.getId().equals(map.get("contractId"))) {
                    contractId.setValue(contractDto);
                }
            }
        }
        if (map.containsKey("subjectId")) {
            for (SubjectDto subjectDto : subjectDtoList) {
                if (subjectDto.getId() != null && subjectDto.getId().equals(map.get("subjectId"))) {
                    subjectId.setValue(subjectDto);
                }
            }
        }
        if (map.containsKey("showDeleted") && SecurityUtils.hasPermission(Permission.CONTACT_PERSON_SHOW_DELETED)) {
            showDeleted.setValue("true".equalsIgnoreCase(map.get("showDeleted")));
        }
        if (map.containsKey("onlyEditPermission")) {
            onlyEditPermission.setValue("true".equalsIgnoreCase(map.get("onlyEditPermission")));
        } else {
            onlyEditPermission.setValue(true);
        }
        if (map.containsKey("transferProtocol")) {
            transferProtocol.setValue(FilterBoolean.getValueForFilter(map.get("transferProtocol")));
        }
    }

    public void fillUrl() {
        String paramUrl = InvoicingView.ROUTE.concat("/");
        if (!payed.getValue().equals(FilterBoolean.ALL)) {
            paramUrl = paramUrl.concat("&payed=").concat(payed.getValue().getValue().toString());
        }
        if (invoicingDateStart.getValue() != null) {
            paramUrl = paramUrl.concat("&invoicingDateStart=").concat(invoicingDateStart.getValue().toString());
        }
        if (invoicingDateEnd.getValue() != null) {
            paramUrl = paramUrl.concat("&invoicingDateEnd=").concat(invoicingDateEnd.getValue().toString());
        }
        if (userDto.getValue() != null && userDto.getValue().getId() != null) {
            paramUrl = paramUrl.concat("&user=").concat(String.valueOf(userDto.getValue().getId()));
        }
        if (issueDateStart.getValue() != null) {
            paramUrl = paramUrl.concat("&issueDateStart=").concat(issueDateStart.getValue().toString());
        }
        if (issueDateEnd.getValue() != null) {
            paramUrl = paramUrl.concat("&issueDateEnd=").concat(issueDateEnd.getValue().toString());
        }
        if (invoiceState.getValue() != null) {
            paramUrl = paramUrl.concat("&invoiceState=").concat(invoiceState.getValue().name());
        }
        if (contractId.getValue() != null && contractId.getValue().getId() != null) {
            paramUrl = paramUrl.concat("&contractId=").concat(contractId.getValue().getId());
        }
        if (subjectId.getValue() != null && subjectId.getValue().getId() != null) {
            paramUrl = paramUrl.concat("&subjectId=").concat(subjectId.getValue().getId());
        }
        if (showDeleted.getValue() != null && SecurityUtils.hasPermission(Permission.CONTACT_PERSON_SHOW_DELETED)) {
            paramUrl = paramUrl.concat("&showDeleted=").concat(showDeleted.getValue().toString());
        }
        if (onlyEditPermission.getValue() != null) {
            paramUrl = paramUrl.concat("&onlyEditPermission=").concat(onlyEditPermission.getValue().toString());
        }
        if (!transferProtocol.getValue().equals(FilterBoolean.ALL)) {
            paramUrl = paramUrl.concat("&transferProtocol=").concat(transferProtocol.getValue().getValue().toString());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public InvoiceFilterDto getInvoiceFilterDto() {
        InvoiceFilterDto dto = new InvoiceFilterDto();
        dto.setPayed(payed.getValue());
        dto.setInvoicingDateStart(invoicingDateStart.getValue());
        dto.setInvoicingDateEnd(invoicingDateEnd.getValue());
        if (dto.getUserDto() != null && dto.getUserDto().getId() != 0) {
            dto.setUserDto(userDto.getValue());
        }
        dto.setIssueDateStart(issueDateStart.getValue());
        dto.setIssueDateEnd(issueDateEnd.getValue());
        dto.setInvoiceState(invoiceState.getValue());
        dto.setContractId(contractId.getValue().getId());
        dto.setSubjectId(subjectId.getValue().getId());
        dto.setShowDeleted(Boolean.TRUE.equals(showDeleted.getValue()));
        dto.setOnlyEditPermission(onlyEditPermission.getValue());
        dto.setTransferProtocol(transferProtocol.getValue());
        return dto;
    }
}
