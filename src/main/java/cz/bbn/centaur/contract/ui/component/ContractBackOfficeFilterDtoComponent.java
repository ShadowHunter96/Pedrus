package cz.bbn.cerberus.contract.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.AppAdvancedFilter;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractFilterDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.ui.ContractSalesView;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import org.apache.commons.lang3.StringUtils;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ContractBackOfficeFilterDtoComponent extends AppAdvancedFilter {

    private ComboBox<SubjectDto> customer;
    private MultiSelectComboBox<EnumerationDto> contractState;
    private ComboBox<ContractTypeDto> contractType;
    private ComboBox<SubjectDto> contractParty;
    private TextField id;
    private TextField name;
    private Checkbox showEmptyValidityStart;
    private ComboBox<UserDto> user;
    private MultiSelectComboBox<AreaDto> area;
    private MultiSelectComboBox<TechnologyDto> technology;
    private DatePicker validityStartFrom;
    private DatePicker validityStartTo;
    private DatePicker endOfContractFrom;
    private DatePicker endOfContractTo;
    private DatePicker effectiveDateFrom;
    private DatePicker effectiveDateTo;
    private Checkbox showDeleted;
    private Checkbox onlyEditPermission;

    private final List<SubjectDto> customerDtoList;
    private final List<EnumerationDto> contractStateDtoList;
    private final List<ContractTypeDto> contractTypeDtoList;
    private final List<SubjectDto> contractPartyDtoList;
    private final List<UserDto> userList;
    private final List<AreaDto> areaDtoList;
    private final List<TechnologyDto> technologyDtoList;
    private final AppEnv appEnv;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final ContractInternalType contractInternalType;

    public ContractBackOfficeFilterDtoComponent(Button search, List<SubjectDto> customerDtoList,
                                                List<EnumerationDto> contractStateDtoList,
                                                List<ContractTypeDto> contractTypeDtoList,
                                                List<SubjectDto> contractPartyDtoList, List<UserDto> userList,
                                                List<AreaDto> areaDtoList, List<TechnologyDto> technologyDtoList,
                                                AppEnv appEnv, String params,
                                                HistoryBreadcrumbs historyBreadcrumbs,
                                                ContractInternalType contractInternalType) {
        super(search);
        this.customerDtoList = customerDtoList;
        this.contractStateDtoList = contractStateDtoList;
        this.contractTypeDtoList = contractTypeDtoList;
        this.contractPartyDtoList = contractPartyDtoList;
        this.userList = userList;
        this.areaDtoList = areaDtoList;
        this.technologyDtoList = technologyDtoList;
        this.appEnv = appEnv;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.contractInternalType = contractInternalType;

        initComponents();
    }

    private void initComponents() {

        contractParty = new ComboBox<>(Transl.get("Provider"));
        ComboBox.ItemFilter<SubjectDto> contractPartyFilter = (item, filterText) ->
                StringUtils.stripAccents(item.getName().toLowerCase())
                        .contains(StringUtils.stripAccents(filterText.toLowerCase()));
        contractParty.setItems(contractPartyFilter, contractPartyDtoList);
        contractParty.setItemLabelGenerator(SubjectDto::getName);
        addToBasicFilter(contractParty);

        contractState = new MultiSelectComboBox<>(Transl.get("Contract state"));
        ComboBox.ItemFilter<EnumerationDto> contractStateFilter = (item, filterText) ->
                StringUtils.stripAccents(item.getName().toLowerCase())
                        .contains(StringUtils.stripAccents(filterText.toLowerCase()));
        contractState.setItems(contractStateFilter, contractStateDtoList);
        contractState.setItemLabelGenerator(EnumerationDto::getName);
        addToBasicFilter(contractState);

        contractType = new ComboBox<>(Transl.get("Contract type"));
        ComboBox.ItemFilter<ContractTypeDto> contractTypeFilter = (item, filterText) ->
                StringUtils.stripAccents(item.getName().toLowerCase())
                        .contains(StringUtils.stripAccents(filterText.toLowerCase()));
        contractType.setItems(contractTypeFilter, contractTypeDtoList);
        contractType.setItemLabelGenerator(ContractTypeDto::getName);
        addToBasicFilter(contractType);

        customer = new ComboBox<>(Transl.get("Customer"));
        SubjectDto defaultContractParty = customerDtoList.stream()
                .filter(subjectDto -> subjectDto.getId().equals(appEnv.getDefaultCompanySubject()))
                .findAny().orElse(null);
        customer.setItems(customerDtoList);
        customer.setItemLabelGenerator(SubjectDto::getName);
        if (params == null) {
            customer.setValue(defaultContractParty);
        }
        addToBasicFilter(customer);


        id = new TextField(Transl.get("Contract number"));
        addToBasicFilter(id);

        name = new TextField(Transl.get("Name"));
        addToBasicFilter(name);

        onlyEditPermission = new Checkbox(Transl.get("Only editable"));
        addToBasicFilter(onlyEditPermission);

        validityStartFrom = VaadinComponents.getDatePicker(Transl.get("Validity from"), null);
        addToAdvancedFilter(validityStartFrom);

        validityStartTo = VaadinComponents.getDatePicker(Transl.get("Validity to"), null);
        addToAdvancedFilter(validityStartTo);

        endOfContractFrom = VaadinComponents.getDatePicker(Transl.get("End of contract from"), null);
        addToAdvancedFilter(endOfContractFrom);

        endOfContractTo = VaadinComponents.getDatePicker(Transl.get("End of contract to"), null);
        addToAdvancedFilter(endOfContractTo);

        effectiveDateFrom = VaadinComponents.getDatePicker(Transl.get("Effective date from"), null);
        addToAdvancedFilter(effectiveDateFrom);

        effectiveDateTo = VaadinComponents.getDatePicker(Transl.get("Effective date to"), null);
        addToAdvancedFilter(effectiveDateTo);

        user = new ComboBox<>(Transl.get("Owner"));
        ComboBox.ItemFilter<UserDto> userFilter = (item, filterText) ->
                StringUtils.stripAccents(item.getName().toLowerCase())
                        .contains(StringUtils.stripAccents(filterText.toLowerCase()));
        user.setItems(userFilter, userList);
        user.setItemLabelGenerator(UserDto::getName);
        addToAdvancedFilter(user);

        area = new MultiSelectComboBox<>(Transl.get("area"));
        ComboBox.ItemFilter<AreaDto> areaFilter = (item, filterText) ->
                StringUtils.stripAccents(item.getName().toLowerCase())
                        .contains(StringUtils.stripAccents(filterText.toLowerCase()));
        area.setItems(areaFilter, areaDtoList);
        area.setItemLabelGenerator(AreaDto::getName);
        addToAdvancedFilter(area);

        technology = new MultiSelectComboBox<>(Transl.get("technology"));
        ComboBox.ItemFilter<TechnologyDto> technologyFilter = (item, filterText) ->
                StringUtils.stripAccents(item.getName().toLowerCase())
                        .contains(StringUtils.stripAccents(filterText.toLowerCase()));
        technology.setItems(technologyFilter, technologyDtoList);
        technology.setItemLabelGenerator(TechnologyDto::getName);
        addToAdvancedFilter(technology);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        showDeleted.setValue(false);
        if (SecurityUtils.hasPermission(Permission.CONTRACT_SHOW_DELETED)) {
            addToAdvancedFilter(showDeleted);
        }

        showEmptyValidityStart = new Checkbox(Transl.get("Empty validity start"));
        addToAdvancedFilter(showEmptyValidityStart);

        initFilter();
        fillFilterFromUrl();
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);

        if (map.containsKey("customer") && SecurityUtils.hasCustomPermission(
                DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), map.get("customer"),
                Permission.CUSTOM_PERMISSION_VIEW.name())) {
            for (SubjectDto subjectDto : customerDtoList) {
                if (subjectDto.getId().equals(map.get("customer"))) {
                    customer.setValue(subjectDto);
                }
            }
        }
        if (map.containsKey("contractState")) {
            String[] actualState = map.get("contractState").split(",");
            Set<EnumerationDto> contractStateSet = new HashSet<>();
            for (String stateStr : actualState) {
                for (EnumerationDto enumerationDto : contractStateDtoList) {
                    if (String.valueOf(enumerationDto.getId()).equals(stateStr)) {
                        contractStateSet.add(enumerationDto);
                    }
                }
            }
            contractState.setValue(contractStateSet);
        }
        if (map.containsKey("contractType")) {
            String stateId = map.get("contractType");
            for (ContractTypeDto type : contractTypeDtoList) {
                if (type.getId().equals(stateId)) {
                    contractType.setValue(type);
                }
            }
        }
        if (map.containsKey("contractParty") && SecurityUtils.hasCustomPermission(
                DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), map.get("contractParty"),
                Permission.CUSTOM_PERMISSION_VIEW.name())) {
            for (SubjectDto subjectDto : contractPartyDtoList) {
                if (subjectDto.getId().equals(map.get("contractParty"))) {
                    contractParty.setValue(subjectDto);
                }
            }
        }
        if (map.containsKey("id")) {
            id.setValue(map.get("id"));
        }
        if (map.containsKey("name")) {
            name.setValue(map.get("name"));
        }
        if (map.containsKey("onlyEditPermission")) {
            onlyEditPermission.setValue("true".equalsIgnoreCase(map.get("onlyEditPermission")));
        } else {
            onlyEditPermission.setValue(true);
        }
        if (map.containsKey("validityStartFrom")) {
            validityStartFrom.setValue(LocalDate.parse(map.get("validityStartFrom")));
        }
        if (map.containsKey("validityStartTo")) {
            validityStartTo.setValue(LocalDate.parse(map.get("validityStartTo")));
        }
        if (map.containsKey("endOfContractFrom")) {
            endOfContractFrom.setValue(LocalDate.parse(map.get("endOfContractFrom")));
        }
        if (map.containsKey("endOfContractTo")) {
            endOfContractTo.setValue(LocalDate.parse(map.get("endOfContractTo")));
        }
        if (map.containsKey("effectiveDateFrom")) {
            effectiveDateFrom.setValue(LocalDate.parse(map.get("effectiveDateFrom")));
        }
        if (map.containsKey("effectiveDateTo")) {
            effectiveDateTo.setValue(LocalDate.parse(map.get("effectiveDateTo")));
        }
        if (map.containsKey("user")) {
            for (UserDto userDto : userList) {
                if (String.valueOf(userDto.getId()).equals(map.get("user"))) {
                    user.setValue(userDto);
                }
            }
        }
        if (map.containsKey("area")) {
            String[] areaStrArr = map.get("area").split(",");
            List<AreaDto> areaList = new ArrayList<>();
            for (String areaStr : areaStrArr) {
                for (AreaDto areaDto : areaDtoList) {
                    if (areaDto.getId().equals(areaStr)) {
                        areaList.add(areaDto);
                    }
                }
            }
            area.setValue(areaList);
        }
        if (map.containsKey("technology")) {
            String[] technologyStrArr = map.get("technology").split(",");
            List<TechnologyDto> technologyList = new ArrayList<>();
            for (String technologyStr : technologyStrArr) {
                for (TechnologyDto technologyDto : technologyDtoList) {
                    if (technologyDto.getId().equals(technologyStr)) {
                        technologyList.add(technologyDto);
                    }
                }
            }
            technology.setValue(technologyList);
        }
        if (map.containsKey("showDeleted") && SecurityUtils.hasPermission(Permission.CONTRACT_SHOW_DELETED)) {
            showDeleted.setValue("true".equalsIgnoreCase(map.get("showDeleted")));
        }
        if (map.containsKey("showEmptyValidityStart")) {
            showEmptyValidityStart.setValue("true".equalsIgnoreCase(map.get("showEmptyValidityStart")));
        }
    }

    public void fillUrl() {
        String paramUrl = ContractSalesView.ROUTE.concat("/");

        if (customer.getValue() != null) {
            paramUrl = paramUrl.concat("&customer=").concat(customer.getValue().getId());
        }
        if (contractState.getValue() != null && !contractState.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&contractState=");
            List<String> stateStrList = new ArrayList<>();
            for (EnumerationDto stateEn : contractState.getValue()) {
                stateStrList.add(String.valueOf(stateEn.getId()));
            }
            paramUrl = paramUrl.concat(String.join(",", stateStrList));
        }
        if (contractType.getValue() != null) {
            paramUrl = paramUrl.concat("&contractType=").concat(contractType.getValue().getId());
        }
        if (contractParty.getValue() != null) {
            paramUrl = paramUrl.concat("&contractParty=").concat(contractParty.getValue().getId());
        }
        if (id.getValue() != null && !id.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&id=").concat(id.getValue());
        }
        if (name.getValue() != null && !name.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&name=").concat(name.getValue());
        }
        if (onlyEditPermission.getValue() != null) {
            paramUrl = paramUrl.concat("&onlyEditPermission=").concat(onlyEditPermission.getValue().toString());
        }
        if (validityStartTo.getValue() != null) {
            paramUrl = paramUrl.concat("&validityStartTo=").concat(validityStartTo.getValue().toString());
        }
        if (validityStartFrom.getValue() != null) {
            paramUrl = paramUrl.concat("&validityStartFrom=").concat(validityStartFrom.getValue().toString());
        }
        if (endOfContractTo.getValue() != null) {
            paramUrl = paramUrl.concat("&endOfContractTo=").concat(endOfContractTo.getValue().toString());
        }
        if (endOfContractFrom.getValue() != null) {
            paramUrl = paramUrl.concat("&endOfContractTo=").concat(endOfContractFrom.getValue().toString());
        }
        if (effectiveDateFrom.getValue() != null) {
            paramUrl = paramUrl.concat("&effectiveDateFrom=").concat(effectiveDateFrom.getValue().toString());
        }
        if (effectiveDateTo.getValue() != null) {
            paramUrl = paramUrl.concat("&effectiveDateTo=").concat(effectiveDateTo.getValue().toString());
        }
        if (user.getValue() != null) {
            paramUrl = paramUrl.concat("&user=").concat(String.valueOf(user.getValue().getId()));
        }
        if (area.getValue() != null && !area.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&area=");
            List<String> areaIdList = new ArrayList<>();
            for (AreaDto areaDto : area.getValue()) {
                areaIdList.add(areaDto.getId());
            }
            paramUrl = paramUrl.concat(String.join(",", areaIdList));
        }
        if (technology.getValue() != null && !technology.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&technology=");
            List<String> technologyIdList = new ArrayList<>();
            for (TechnologyDto technologyDto : technology.getValue()) {
                technologyIdList.add(technologyDto.getId());
            }
            paramUrl = paramUrl.concat(String.join(",", technologyIdList));
        }
        if (showDeleted.getValue() != null && SecurityUtils.hasPermission(Permission.CONTRACT_SHOW_DELETED)) {
            paramUrl = paramUrl.concat("&showDeleted=").concat(showDeleted.getValue().toString());
        }
        if (showEmptyValidityStart.getValue() != null) {
            paramUrl = paramUrl.concat("&showEmptyValidityStart=")
                    .concat(showEmptyValidityStart.getValue().toString());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public ContractFilterDto getContractFilterDto() {
        ContractFilterDto dto = new ContractFilterDto();
        dto.setCustomerDto(customer.getValue());
        dto.setContractState(contractState.getValue());
        dto.setContractTypeDto(contractType.getValue());
        dto.setContractPartyDto(contractParty.getValue());
        dto.setId(id.getValue());
        dto.setName(name.getValue());
        dto.setValidityStartFrom(validityStartFrom.getValue());
        dto.setValidityStartTo(validityStartTo.getValue());
        dto.setEndDateFrom(endOfContractFrom.getValue());
        dto.setEndDateTo(endOfContractTo.getValue());
        dto.setEffectiveStartFrom(effectiveDateFrom.getValue());
        dto.setEffectiveStartTo(effectiveDateTo.getValue());
        dto.setShowEmptyValidityStart(showEmptyValidityStart.getValue());
        dto.setUserDto(user.getValue());
        dto.setAreaDtoSet(area.getValue());
        dto.setTechnologyDtoSet(technology.getValue());
        dto.setShowDeleted(showDeleted.getValue());
        dto.setOnlyEditPermission(onlyEditPermission.getValue());
        dto.setContractInternalType(contractInternalType);
        return dto;
    }
}
