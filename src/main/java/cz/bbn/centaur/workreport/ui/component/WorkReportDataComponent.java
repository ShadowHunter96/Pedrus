package cz.bbn.cerberus.workreport.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.validator.DateMinMaxValidator;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.ProjectPhaseActivityDto;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;
import cz.bbn.cerberus.workreport.dto.WorkReportPickDto;

import java.time.LocalDate;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class WorkReportDataComponent extends FormLayout {

    private final List<WorkReportPickDto> projectList;
    private final Map<String, List<PhaseDto>> projectPhaseMap;
    private final Map<String, List<EnumerationDto>> projectActivityMap;
    private final List<Double> durationList;
    private final Binder<WorkReportDto> binder;
    private final WorkReportSaveListener listener;
    private final WorkReportDto dto;
    private final boolean showSubmit;
    private final boolean enableDateChange;
    private final List<HolidayEntity> holidayEntityList;

    private DatePicker date;
    private ComboBox<WorkReportPickDto> pickItem;
    private ComboBox<PhaseDto> phase;
    private ComboBox<EnumerationDto> activity;
    private ComboBox<Double> duration;
    private TextArea description;
    private Button submit;

    public WorkReportDataComponent(ProjectPhaseActivityDto projectPhaseActivityDto, List<Double> durationList,
                                   Binder<WorkReportDto> binder, WorkReportSaveListener listener,
                                   WorkReportDto dto, boolean showSubmit, boolean enableDateChange, List<HolidayEntity> holidayEntityList) {
        this.projectList = projectPhaseActivityDto.getPickItemList();
        this.projectPhaseMap = projectPhaseActivityDto.getProjectPhaseMap();
        this.projectActivityMap = projectPhaseActivityDto.getProjectActivityMap();
        this.durationList = durationList;
        this.binder = binder;
        this.listener = listener;
        this.dto = dto;
        this.showSubmit = showSubmit;
        this.enableDateChange = enableDateChange;
        this.holidayEntityList = holidayEntityList;
        projectList.sort(Comparator.comparing(WorkReportPickDto::getName));
        initComponent();
    }

    private void initComponent() {

        setWidthFull();

        date = VaadinComponents.getDatePicker(null);
        date.setLabel(Transl.get("Date"));
        binder.forField(date).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .withValidator(new DateMinMaxValidator(getMinDate(dto.getDate()), getMaxDate(dto.getDate())))
                .bind(WorkReportDto::getDate, WorkReportDto::setDate);
        this.add(date);

        if (!enableDateChange) {
            date.setReadOnly(true);
        }

        pickItem = new ComboBox<>(Transl.get("Project"));
        pickItem.setItems(projectList);
        pickItem.setItemLabelGenerator(workReportPickDto -> Optional.ofNullable(workReportPickDto.getName()).orElse(""));
        binder.forField(pickItem).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(WorkReportDto::getPickDto, WorkReportDto::setPickDto);
        this.add(pickItem);

        phase = new ComboBox<>(Transl.get("Project phase"));
        phase.setItems(new ArrayList<>());
        phase.setItemLabelGenerator(PhaseDto::getName);
        binder.forField(phase).bind(WorkReportDto::getPhaseDto, WorkReportDto::setPhaseDto);
        this.add(phase);

        activity = new ComboBox<>(Transl.get("Activity"));
        activity.setItems(new ArrayList<>());
        activity.setItemLabelGenerator(EnumerationDto::getName);
        binder.forField(activity).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(WorkReportDto::getActivity, WorkReportDto::setActivity);
        this.add(activity);

        duration = new ComboBox<>(Transl.get("Duration"));
        duration.setItems(durationList);
        binder.forField(duration).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(WorkReportDto::getDuration, WorkReportDto::setDuration);
        this.add(duration);

        description = new TextArea(Transl.get("Description"));
        description.setHeight("5em");
        description.setMaxHeight("5em");
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        binder.forField(description)
                    .bind(WorkReportDto::getDescription, WorkReportDto::setDescription);

        this.add(description, 2);

        submit = VaadinComponents.getSubmitButton();
        if (showSubmit && dto.getApprovementDto() == null) {
            submit.addClickListener(e -> listener.save(binder, null, null, true, holidayEntityList));
            this.add(submit);
        }

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);

        pickItem.addValueChangeListener(e -> {
            if (e.getValue() != null && projectPhaseMap.containsKey(e.getValue().getId())) {
                phase.setItems(projectPhaseMap.get(e.getValue().getId()).stream()
                        .sorted(Comparator.comparing(PhaseDto::getName)).toList());

                activity.setItems(projectActivityMap.get(e.getValue().getId()).stream()
                        .sorted(Comparator.comparing(EnumerationDto::getName)).toList());
            } else {
                phase.setItems(new ArrayList<>());
                activity.setItems(new ArrayList<>());
            }
            phase.setValue(null);
            activity.setValue(null);
        });
    }

    private LocalDate getMinDate(LocalDate localDate) {
        YearMonth month = YearMonth.from(localDate);
        return month.atDay(1);
    }

    private LocalDate getMaxDate(LocalDate localDate) {
        YearMonth month = YearMonth.from(localDate);
        return month.atEndOfMonth();
    }

    public void setProjectPhase(WorkReportPickDto projectDto, PhaseDto phaseDto) {
        pickItem.setValue(projectDto);
        phase.setValue(phaseDto);
        activity.setValue(null);
    }

    public void setComponentEnabled(boolean enabled) {
        if (showSubmit) {
            date.setVisible(enabled);
            pickItem.setVisible(enabled);
            phase.setVisible(enabled);
            activity.setVisible(enabled);
            duration.setVisible(enabled);
            description.setVisible(enabled);
            submit.setEnabled(enabled);
            submit.setVisible(enabled);
        } else {
            date.setReadOnly(true);
            pickItem.setReadOnly(true);
            phase.setReadOnly(true);
            activity.setReadOnly(true);
            duration.setReadOnly(true);
            description.setReadOnly(true);
        }
        submit.setVisible(enabled);
    }

}
