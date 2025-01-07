package cz.bbn.cerberus.workreport.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.ConfirmDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.DayWorkReportDto;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;

import java.time.format.TextStyle;
import java.util.List;
import java.util.Locale;

public class WorkReportGridComponent extends Grid<DayWorkReportDto> {

    private static final String ROW_HEIGHT = "2.7em";

    private final List<DayWorkReportDto> dayWorkReportDtoList;
    private final WorkReportListener listener;
    private final AppEnv appEnv;

    private boolean enableActions = true;

    public WorkReportGridComponent(List<DayWorkReportDto> dayWorkReportDtoList, WorkReportListener listener,
                                   AppEnv appEnv) {
        this.dayWorkReportDtoList = dayWorkReportDtoList;
        this.listener = listener;
        this.appEnv = appEnv;
        initGrid();
    }

    private void initGrid() {
        setSizeFull();
        addColumn(new ComponentRenderer<>(this::getHoursTotal)).setHeader(Transl.get("Total")).setFlexGrow(0)
                .setWidth("5.5em").setResizable(false);
        addColumn(new ComponentRenderer<>(this::getFormattedDate)).setHeader(Transl.get("Date")).setFlexGrow(0)
                .setWidth("9em").setResizable(false);
        addColumn(new ComponentRenderer<>(this::getProjectDiv)).setHeader(Transl.get("Project"));
        addColumn(new ComponentRenderer<>(this::getProjectPhaseDiv)).setHeader(Transl.get("Project phase"));
        addColumn(new ComponentRenderer<>(this::getProjectActivityDiv)).setHeader(Transl.get("Activity"));
        addColumn(new ComponentRenderer<>(this::getHoursDiv)).setHeader(Transl.get("Hours")).setFlexGrow(0)
                .setWidth("5.5em").setResizable(false);
        addColumn(new ComponentRenderer<>(this::getDescriptionDiv)).setHeader(Transl.get("Description")).setFlexGrow(2);
        addColumn(new ComponentRenderer<>(this::getActionDiv)).setHeader(Transl.get("Actions")).setFlexGrow(0)
                .setTextAlign(ColumnTextAlign.CENTER).setWidth("8.5em").setResizable(false);

        setItems(dayWorkReportDtoList);

    }

    private Span getHoursTotal(DayWorkReportDto dto) {
        if (dto != null) {
            String color = "green";
            if (dto.getHoursTotal() < 8) {
                color = "red";
            }
            Span hourTotalSpan = new Span(String.valueOf(dto.getHoursTotal()));
            hourTotalSpan.getElement().getStyle().set("color", color);
            return hourTotalSpan;
        }
        return new Span();
    }

    private Span getFormattedDate(DayWorkReportDto dto) {
        if (dto != null && dto.getDate() != null) {
            return new Span(AppUtils.formatDayMonth(dto.getDate()) + " " +
                    Transl.get(dto.getDate().getDayOfWeek().getDisplayName(TextStyle.FULL, Locale.ENGLISH)));
        }
        return new Span();
    }

    private VerticalLayout getProjectDiv(DayWorkReportDto dto) {
        VerticalLayout verticalLayout = getBasicLayout();
        verticalLayout.setWidthFull();
        boolean first = true;
        for (WorkReportDto workReportDto : dto.getWorkReportDtoList()) {
            String text = "";
            if (workReportDto.getPickDto() != null) {
                text = workReportDto.getPickDto().getName();
            }
            first = setBlackLine(first, verticalLayout);
            addLineDiv(verticalLayout, text, workReportDto);
        }
        return verticalLayout;
    }

    private VerticalLayout getProjectPhaseDiv(DayWorkReportDto dto) {
        VerticalLayout verticalLayout = getBasicLayout();
        boolean first = true;
        for (WorkReportDto workReportDto : dto.getWorkReportDtoList()) {
            String text = "";
            if (workReportDto.getPhaseDto() != null) {
                text = workReportDto.getPhaseDto().getName();
            }
            first = setBlackLine(first, verticalLayout);
            addLineDiv(verticalLayout, text, workReportDto);
        }
        return verticalLayout;
    }

    private VerticalLayout getProjectActivityDiv(DayWorkReportDto dto) {
        VerticalLayout verticalLayout = getBasicLayout();
        boolean first = true;
        for (WorkReportDto workReportDto : dto.getWorkReportDtoList()) {
            String text = "";
            if (workReportDto.getActivity() != null) {
                text = workReportDto.getActivity().getName();
            }
            first = setBlackLine(first, verticalLayout);
            addLineDiv(verticalLayout, text, workReportDto);
        }
        return verticalLayout;
    }

    private VerticalLayout getHoursDiv(DayWorkReportDto dto) {
        VerticalLayout verticalLayout = getBasicLayout();
        boolean first = true;
        for (WorkReportDto workReportDto : dto.getWorkReportDtoList()) {
            String text = "";
            if (workReportDto.getDuration() != null) {
                text = String.valueOf(workReportDto.getDuration());
            }
            first = setBlackLine(first, verticalLayout);
            addLineDiv(verticalLayout, text, workReportDto);
        }
        return verticalLayout;
    }

    private VerticalLayout getDescriptionDiv(DayWorkReportDto dto) {
        VerticalLayout verticalLayout = getBasicLayout();
        boolean first = true;
        for (WorkReportDto workReportDto : dto.getWorkReportDtoList()) {
            String text = workReportDto.getDescription();
            first = setBlackLine(first, verticalLayout);
            addLineDiv(verticalLayout, text, workReportDto);
        }
        return verticalLayout;
    }

    private VerticalLayout getActionDiv(DayWorkReportDto dto) {
        VerticalLayout verticalLayout = getBasicLayout();
        boolean first = true;
        for (WorkReportDto workReportDto : dto.getWorkReportDtoList()) {
            first = setBlackLine(first, verticalLayout);
            verticalLayout.add(getGridButtons(workReportDto));
        }
        return verticalLayout;
    }

    private VerticalLayout getBasicLayout() {
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setSpacing(false);
        verticalLayout.setMargin(false);
        verticalLayout.setPadding(false);
        return verticalLayout;
    }

    private HorizontalLayout getGridButtons(WorkReportDto dto) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setHeight(ROW_HEIGHT);
        buttons.setWidthFull();

        buttons.setMargin(false);
        buttons.setPadding(false);

        Button delete = VaadinComponents.getDeleteButton();
        AppUtils.addRfClassToGridButton(delete, String.valueOf(dto.getId()));
        delete.addClickListener(buttonClickEvent -> {
            if (enableActions) {
                ConfirmDialog deleteConfirmDialog = new ConfirmDialog(
                        Transl.get("Delete work report?"), listener.getConfirmAction(dto));
                deleteConfirmDialog.open();
            } else {
                ErrorNotification.show(Transl.get("Work report for this month is closed"), appEnv);
            }
        });
        delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete work report"));
        buttons.add(delete);

        buttons.addClickListener(event -> {
            if (event.getClickCount() > 1) {
                listener.openEditDialog(dto, enableActions);
            }
        });

        return buttons;
    }

    private void addLineDiv(VerticalLayout verticalLayout, String text, WorkReportDto dto) {
        Div innerDiv = new Div();
        innerDiv.getElement().getStyle().set("overflow", "hidden").set("text-overflow", "ellipsis")
                .set("line-height", ROW_HEIGHT);
        innerDiv.getElement().setProperty(TextValues.TITLE, text);
        innerDiv.setText(text);
        innerDiv.setWidthFull();
        innerDiv.setHeight(ROW_HEIGHT);
        innerDiv.setId(String.valueOf(dto.getId()));
        verticalLayout.add(innerDiv);
        innerDiv.addClickListener(event -> {
            if (event.getClickCount() > 1) {
                listener.openEditDialog(dto, enableActions);
            }
        });
    }

    private Span getBlackLine() {
        Span span = new Span();
        span.getElement().getStyle().set("background-color", "var(--item-selected-color)").set("height", "1px")
                .set("width", "calc(100% + 6em)").set("margin-left", "-3em");
        return span;
    }

    private boolean setBlackLine(boolean first, VerticalLayout verticalLayout) {
        if (!first) {
            verticalLayout.add(getBlackLine());
        }
        return false;
    }

    public void setEnableActions(boolean enableActions) {
        this.enableActions = enableActions;
    }

}
