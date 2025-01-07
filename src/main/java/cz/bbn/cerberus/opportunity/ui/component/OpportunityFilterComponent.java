package cz.bbn.cerberus.opportunity.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.AppDatePicker;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enums.PctValues;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.opportunity.dto.OpportunityDtoFilter;
import cz.bbn.cerberus.opportunity.dto.OpportunityState;
import cz.bbn.cerberus.opportunity.ui.OpportunityView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class OpportunityFilterComponent extends FormLayout {

    private TextField name;
    private Checkbox showDeleted;
    private MultiSelectComboBox<SubjectDto> subject;
    private MultiSelectComboBox<OpportunityState> state;
    private MultiSelectComboBox<PctValues> progress;
    private MultiSelectComboBox<PctValues> successChance;
    private DatePicker startDateFrom;
    private DatePicker startDateTo;
    private Checkbox onlyEditPermission;

    private final Button search;
    private final List<SubjectDto> subjectDtoList;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;

    public OpportunityFilterComponent(Button search, List<SubjectDto> subjectDtoList, String params,
                                      HistoryBreadcrumbs historyBreadcrumbs) {
        this.search = search;
        this.subjectDtoList = subjectDtoList;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        initComponent();
    }

    private void initComponent() {
        name = new TextField(Transl.get("Name"));
        this.add(name);

        subject = new MultiSelectComboBox<>(Transl.get("Customer"));
        subject.setItemLabelGenerator(SubjectDto::getName);
        subject.setItems(subjectDtoList);
        this.add(subject);

        state = new MultiSelectComboBox<>(Transl.get("State"));
        state.setItemLabelGenerator(actualOpportunityState -> Transl.get(actualOpportunityState.name()));
        state.setItems(OpportunityState.values());
        this.add(state);

        progress = new MultiSelectComboBox<>(Transl.get("Progress"));
        progress.setItemLabelGenerator(PctValues::getValue);
        progress.setItems(PctValues.values());
        this.add(progress);

        successChance = new MultiSelectComboBox<>(Transl.get("Success chance"));
        successChance.setItemLabelGenerator(PctValues::getValue);
        successChance.setItems(PctValues.values());
        this.add(successChance);

        startDateFrom = new AppDatePicker(Transl.get("Realization from"), null);
        this.add(startDateFrom);

        startDateTo = new AppDatePicker(Transl.get("Realization to"), null);
        this.add(startDateTo);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.ASSET_SHOW_DELETED)) {
            this.add(showDeleted);
        }

        onlyEditPermission = new Checkbox(Transl.get("Only editable"));
        this.add(onlyEditPermission);

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
        fillFilterFromUrl();
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("name")) {
            name.setValue(map.get("name"));
        }
        if (map.containsKey("subject")) {
            String[] subjectIdList = map.get("subject").split(",");
            List<SubjectDto> subjectList = new ArrayList<>();
            for (String subjectId : subjectIdList) {
                for (SubjectDto subjectDto : subjectDtoList) {
                    if (subjectDto.getId().equals(subjectId)) {
                        subjectList.add(subjectDto);
                    }
                }
            }
            subject.setValue(subjectList);
        }
        if (map.containsKey("state")) {
            String[] actualState = map.get("state").split(",");
            Set<OpportunityState> opportunityStateSet = new HashSet<>();
            for (String stateStr : actualState) {
                if (OpportunityState.stateExist(stateStr)) {
                    opportunityStateSet.add(OpportunityState.valueOf(stateStr));
                }
            }
            state.setValue(opportunityStateSet);
        }
        if (map.containsKey("progress")) {
            String[] progressList = map.get("progress").split(",");
            List<PctValues> pctList = new ArrayList<>();
            for (String pct : progressList) {
                if (PctValues.exists(pct)) {
                    pctList.add(PctValues.valueOf(pct));
                }
            }
            progress.setValue(pctList);
        }
        if (map.containsKey("successChance")) {
            String[] progressList = map.get("successChance").split(",");
            List<PctValues> pctList = new ArrayList<>();
            for (String pct : progressList) {
                if (PctValues.exists(pct)) {
                    pctList.add(PctValues.valueOf(pct));
                }
            }
            successChance.setValue(pctList);
        }
        if (map.containsKey("startDateFrom")) {
            startDateFrom.setValue(LocalDate.parse(map.get("startDateFrom")));
        }
        if (map.containsKey("startDateTo")) {
            startDateTo.setValue(LocalDate.parse(map.get("startDateTo")));
        }
        if (map.containsKey("showDeleted") && SecurityUtils.hasPermission(Permission.ASSET_SHOW_DELETED)) {
            showDeleted.setValue("true".equalsIgnoreCase(map.get("showDeleted")));
        }
        if (map.containsKey("onlyEditPermission")) {
            onlyEditPermission.setValue("true".equalsIgnoreCase(map.get("onlyEditPermission")));
        } else {
            onlyEditPermission.setValue(true);
        }
    }

    public void fillUrl() {
        String paramUrl = OpportunityView.ROUTE.concat("/");
        if (name.getValue() != null && !name.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&name=").concat(name.getValue());
        }
        if (subject.getValue() != null && !subject.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&subject=");
            List<String> subjectIdList = new ArrayList<>();
            for (SubjectDto subjectDto : subject.getValue()) {
                subjectIdList.add(subjectDto.getId());
            }
            paramUrl = paramUrl.concat(String.join(",", subjectIdList));
        }
        if (state.getValue() != null && !state.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&state=");
            List<String> stateStrList = new ArrayList<>();
            for (OpportunityState stateEn : state.getValue()) {
                stateStrList.add(stateEn.name());
            }
            paramUrl = paramUrl.concat(String.join(",", stateStrList));
        }
        if (progress.getValue() != null && !progress.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&progress=");
            List<String> progressStrList = new ArrayList<>();
            for (PctValues progressPct : progress.getValue()) {
                progressStrList.add(progressPct.name());
            }
            paramUrl = paramUrl.concat(String.join(",", progressStrList));
        }
        if (successChance.getValue() != null && !successChance.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&successChance=");
            List<String> successChanceStrList = new ArrayList<>();
            for (PctValues successPct : successChance.getValue()) {
                successChanceStrList.add(successPct.name());
            }
            paramUrl = paramUrl.concat(String.join(",", successChanceStrList));
        }
        if (startDateFrom.getValue() != null) {
            paramUrl = paramUrl.concat("&startDateFrom=").concat(startDateFrom.getValue().toString());
        }
        if (startDateTo.getValue() != null) {
            paramUrl = paramUrl.concat("&startDateTo=").concat(startDateTo.getValue().toString());
        }
        if (showDeleted.getValue() != null && SecurityUtils.hasPermission(Permission.ASSET_SHOW_DELETED)) {
            paramUrl = paramUrl.concat("&showDeleted=").concat(showDeleted.getValue().toString());
        }
        if (onlyEditPermission.getValue() != null) {
            paramUrl = paramUrl.concat("&onlyEditPermission=").concat(onlyEditPermission.getValue().toString());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public OpportunityDtoFilter getOpportunityDtoFilter() {
        OpportunityDtoFilter opportunityDtoFilter = new OpportunityDtoFilter();
        opportunityDtoFilter.setName(name.getValue());
        opportunityDtoFilter.setShowDeleted(showDeleted.getValue());
        opportunityDtoFilter.setSubjectDtoSet(subject.getValue());
        opportunityDtoFilter.setStateSet(state.getValue());
        opportunityDtoFilter.setProgressSet(progress.getValue());
        opportunityDtoFilter.setSuccessChanceSet(successChance.getValue());
        opportunityDtoFilter.setStartDateFrom(startDateFrom.getValue());
        opportunityDtoFilter.setStartDateTo(startDateTo.getValue());
        opportunityDtoFilter.setOnlyEditPermission(onlyEditPermission.getValue());
        return opportunityDtoFilter;
    }
}
