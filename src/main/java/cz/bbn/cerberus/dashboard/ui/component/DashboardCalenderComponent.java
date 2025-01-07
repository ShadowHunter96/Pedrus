package cz.bbn.cerberus.dashboard.ui.component;

import com.vaadin.flow.component.AbstractField;
import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.azure.dto.OutlookEventDto;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.layouts.CenteredHorizontalLayout;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.dashboard.dto.CalendarMonthDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.project.ui.ProjectDetailView;
import cz.bbn.cerberus.task.dto.TaskDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.DayWorkReportDto;
import org.vaadin.stefan.fullcalendar.CalendarViewImpl;
import org.vaadin.stefan.fullcalendar.Entry;
import org.vaadin.stefan.fullcalendar.FullCalendar;
import org.vaadin.stefan.fullcalendar.FullCalendarBuilder;
import org.vaadin.stefan.fullcalendar.dataprovider.EntryProvider;
import org.vaadin.stefan.fullcalendar.dataprovider.LazyInMemoryEntryProvider;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class DashboardCalenderComponent extends VerticalLayout {

    private static final String EVENT_TYPE = "event-type";

    private final DashboardListener listener;
    private final EmployeeDto employeeDto;
    private final Map<String, ContractDto> contractMap;

    private final Checkbox showProjects = new Checkbox(Transl.get("Show projects"));
    private final Checkbox showInvoices = new Checkbox(Transl.get("Show invoices"));
    private final Checkbox showHolidays = new Checkbox(Transl.get("Show holidays"));
    private final Checkbox showEvents = new Checkbox(Transl.get("Show events"));
    private final Checkbox showOutlookEvents = new Checkbox(Transl.get("Show Outlook events"));
    private final Checkbox showWorkReport = new Checkbox(Transl.get("Show work report"));
    private final DatePicker date = VaadinComponents.getDatePicker(LocalDate.now());

    private List<ProjectDto> projectDtoList;
    private List<InvoiceDto> invoiceDtoList;
    private List<HolidayEntity> holidayEntityList;
    private List<TaskDto> taskDtoList;
    private List<OutlookEventDto> outlookEventList;

    private LazyInMemoryEntryProvider<Entry> entryMonthProvider;
    private LazyInMemoryEntryProvider<Entry> entryDayProvider;
    private List<DayWorkReportDto> dayWorkReportList;

    private String currentYearMonth;

    public DashboardCalenderComponent(CalendarMonthDto calendarMonthDto, Map<String, ContractDto> contractMap,
                                      EmployeeDto employeeDto, DashboardListener listener) {
        this.contractMap = contractMap;
        this.employeeDto = employeeDto;
        this.listener = listener;
        showProjects.setValue(true);
        showInvoices.setValue(true);
        showHolidays.setValue(true);
        showEvents.setValue(true);
        showOutlookEvents.setValue(true);
        showWorkReport.setValue(false);
        setLists(calendarMonthDto);
        initComponent();
        initCheckboxes();
    }

    private void initComponent() {
        removeAll();
        setSizeFull();

        LocalDate now = LocalDate.now();
        currentYearMonth = now.getYear() + now.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH);

        entryMonthProvider = getEntryProvider();
        FullCalendar fullCalendar = getCalender(entryMonthProvider);

        fullCalendar.gotoDate(date.getValue());

        Button nextButton = VaadinComponents.getButton(VaadinIcon.ARROW_RIGHT.create());
        nextButton.getElement().setProperty(TextValues.TITLE, Transl.get("Next"));
        nextButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        nextButton.setIconAfterText(true);

        Button prevButton = VaadinComponents.getButton(VaadinIcon.ARROW_LEFT.create());
        prevButton.getElement().setProperty(TextValues.TITLE, Transl.get("Previous"));
        prevButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);

        date.setLocale(new Locale("cs_CZ"));

        prevButton.addClickListener(e -> date.setValue(date.getValue().minusMonths(1)));
        nextButton.addClickListener(e -> date.setValue(date.getValue().plusMonths(1)));

        CenteredHorizontalLayout centeredHorizontalLayout = new CenteredHorizontalLayout(prevButton, date, nextButton);

        HorizontalLayout content = new HorizontalLayout();
        content.setMargin(false);
        content.setPadding(false);
        content.setSizeFull();
        content.addClassName("calender-horizontal-layout");

        fullCalendar.addTimeslotClickedListener(e -> date.setValue(e.getDate()));
        fullCalendar.addDayNumberClickedListener(e -> date.setValue(e.getDate()));

        FormLayout checkBoxLayout = new FormLayout();
        checkBoxLayout.setWidthFull();
        checkBoxLayout.add(showProjects, showInvoices, showHolidays, showEvents);
        if (employeeDto != null) {
            checkBoxLayout.add(showWorkReport);
        }
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(checkBoxLayout);

        content.add(fullCalendar);
        content.setFlexGrow(3, fullCalendar);

        entryDayProvider = getEntryProvider();
        FullCalendar dayCalendar = getCalender(entryDayProvider);
        dayCalendar.changeView(CalendarViewImpl.DAY_GRID_DAY);
        dayCalendar.addClassName("calender-daily-agenda");

        date.addValueChangeListener(e -> {
            LocalDate value = e.getValue();
            dayCalendar.gotoDate(value);
            fullCalendar.gotoDate(value);
            String yearMonth = value.getYear() + value.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH);
            if (!Objects.equals(currentYearMonth, yearMonth)) {
                this.currentYearMonth = yearMonth;
                setLists(listener.getMonthData(value));
                refreshCalendar();
            }
        });

        content.add(dayCalendar);
        content.setFlexGrow(1, dayCalendar);

        this.add(centeredHorizontalLayout, checkBoxLayout, content);
    }

    public void setDayWorkReportList(List<DayWorkReportDto> dayWorkReportList) {
        this.dayWorkReportList = dayWorkReportList;
    }

    public void refreshCalendar() {
        refreshEntries(entryMonthProvider);
        refreshEntries(entryDayProvider);
    }

    public LocalDate getCurrentDate() {
        return date.getValue();
    }

    public void setLists(CalendarMonthDto calendarMonthDto) {
        this.projectDtoList = calendarMonthDto.getProjectDtoList();
        this.invoiceDtoList = calendarMonthDto.getInvoiceDtoList();
        this.holidayEntityList = calendarMonthDto.getHolidayEntityList();
        this.taskDtoList = calendarMonthDto.getTaskDtoList();
        this.outlookEventList = calendarMonthDto.getOutlookEventList();
        this.dayWorkReportList = calendarMonthDto.getDayWorkReportList();
    }

    private FullCalendar getCalender(LazyInMemoryEntryProvider<Entry> entryProvider) {
        FullCalendar fullCalendar = FullCalendarBuilder.create().build();
        fullCalendar.setFirstDay(DayOfWeek.MONDAY);
        fullCalendar.setSizeFull();
        fullCalendar.setLocale(Locale.forLanguageTag(Transl.DEFAULT_LANG.toLowerCase()));
        fullCalendar.setEntryProvider(entryProvider);
        fullCalendar.setEntryDurationEditable(false);
        fullCalendar.addEntryClickedListener(entry -> {
                    if ("PROJECT".equals(entry.getEntry().getCustomProperty(EVENT_TYPE))) {
                        UI.getCurrent().navigate(ProjectDetailView.ROUTE + "/"
                                + entry.getEntry().getCustomProperty("project-id").toString()
                        );
                    }
                    if ("WORK-REPORT".equals(entry.getEntry().getCustomProperty(EVENT_TYPE))) {
                        String stringDay = entry.getEntry().getCustomProperty("report-date").toString();
                        String[] listDay = stringDay.split("-");
                        if (listDay.length == 3) {
                            LocalDate day = LocalDate.of(Integer.parseInt(listDay[0]),
                                    Integer.parseInt(listDay[1]), Integer.parseInt(listDay[2]));
                            listener.openWorkReport(day);
                        }
                    }
                }
        );
        return fullCalendar;
    }

    private List<Entry> getProjectEntryList() {
        List<Entry> entryList = new ArrayList<>();
        for (ProjectDto projectDto : projectDtoList) {
            if (projectDto.getStartTime() != null && projectDto.getEndTime() != null) {
                Entry entry = new Entry();
                entry.setStart(projectDto.getStartTime());
                entry.setEnd(projectDto.getEndTime().plusDays(1));
                entry.setBackgroundColor(projectDto.getColor());
                entry.setColor("#112A46");
                entry.setTextColor("#112A46");
                entry.setTitle(projectDto.getName());
                entry.setCustomProperty(EVENT_TYPE, "PROJECT");
                entry.setCustomProperty("project-id", projectDto.getId());
                entry.setDurationEditable(false);
                entry.setStartEditable(false);
                entry.setDescription(projectDto.getDescription());
                entry.assignClassNames("cursor-pointer");
                entry.setCustomProperty("cursor", "pointer");
                Set<String> classNames = new HashSet<>();
                classNames.add("cursor-pointer");
                entry.setClassNames(classNames);
                entryList.add(entry);
            }
        }
        return entryList;
    }

    private List<Entry> getInvoiceEntryList() {
        List<Entry> entryList = new ArrayList<>();
        for (InvoiceDto invoiceDto : invoiceDtoList) {
            if (invoiceDto.getIssueDate() != null && invoiceDto.getDaysToPay() != null) {
                Entry entry = new Entry();
                entry.setStart(invoiceDto.getIssueDate().plusDays(invoiceDto.getDaysToPay())
                        .atStartOfDay().withHour(9));
                entry.setEnd(invoiceDto.getIssueDate().plusDays(invoiceDto.getDaysToPay()).atStartOfDay().withHour(15));
                entry.setTitle(Transl.get("Invoicing") + ": " + getContractName(invoiceDto.getContractDto().getId()));
                entry.setCustomProperty(EVENT_TYPE, "INVOICING");
                entry.setCustomProperty("invoice-id", invoiceDto.getId());
                entry.setDurationEditable(false);
                entry.setStartEditable(false);
                entryList.add(entry);
            }
        }
        return entryList;
    }

    private List<Entry> getHolidayEntryList() {
        List<Entry> entryList = new ArrayList<>();
        for (HolidayEntity holidayEntity : holidayEntityList) {
            Entry entry = new Entry();
            entry.setStart(holidayEntity.getDate().atStartOfDay());
            entry.setEnd(holidayEntity.getDate().atStartOfDay().withHour(23));
            entry.setTitle(holidayEntity.getDescription());
            entry.setCustomProperty(EVENT_TYPE, "HOLIDAY");
            entry.setDurationEditable(false);
            entry.setStartEditable(false);
            entry.setColor("pink");
            entryList.add(entry);
        }
        return entryList;
    }

    private List<Entry> getEventEntryList() {
        List<Entry> entryList = new ArrayList<>();
        for (TaskDto taskDto : taskDtoList) {
            Entry entry = new Entry();
            entry.setStart(taskDto.getDate());
            entry.setEnd(taskDto.getDate());
            entry.setTitle(Transl.get("Event") + ": " + taskDto.getName());
            entry.setCustomProperty(EVENT_TYPE, "EVENT");
            entry.setCustomProperty("event-id", taskDto.getId());
            entry.setDurationEditable(false);
            entry.setStartEditable(false);
            entry.setColor("green");
            entryList.add(entry);
        }
        return entryList;
    }

    private List<Entry> getOutlookEventEntryList() {
        List<Entry> entryList = new ArrayList<>();
        for (OutlookEventDto outlookEventDto : outlookEventList) {
            Entry entry = new Entry();
            entry.setStart(outlookEventDto.getFrom());
            entry.setEnd(outlookEventDto.getTo());
            entry.setTitle(outlookEventDto.getSubject());
            entry.setCustomProperty(EVENT_TYPE, "OUTLOOK-EVENT");
            entry.setDurationEditable(false);
            entry.setStartEditable(false);
            entry.setColor("violet");
            entryList.add(entry);
        }
        return entryList;
    }

    private List<Entry> getWorkReportList() {
        List<Entry> entryList = new ArrayList<>();
        for (DayWorkReportDto dayWorkReportDto : dayWorkReportList) {
            Entry entry = new Entry();
            entry.setStart(LocalDateTime.of(dayWorkReportDto.getDate(), LocalTime.of(9, 0)));
            entry.setEnd(LocalDateTime.of(dayWorkReportDto.getDate(), LocalTime.of(17, 30)));
            entry.setTitle(String.valueOf(dayWorkReportDto.getHoursTotal()));
            entry.setCustomProperty(EVENT_TYPE, "WORK-REPORT");
            entry.setCustomProperty("report-date", dayWorkReportDto.getDate());
            entry.setDurationEditable(false);
            entry.setStartEditable(false);
            entry.setAllDay(true);
            entry.setRenderingMode(Entry.RenderingMode.BLOCK);
            String color = "red";
            if (dayWorkReportDto.getHoursTotal() == 8D) {
                color = "green";
            }
            if (dayWorkReportDto.getHoursTotal() > 8D) {
                color = "blue";
            }
            entry.setColor(color);
            entryList.add(entry);
        }
        return entryList;
    }

    private void initCheckboxes() {
        showProjects.addValueChangeListener(getRefreshEntriesValueChangeListener());
        showHolidays.addValueChangeListener(getRefreshEntriesValueChangeListener());
        showInvoices.addValueChangeListener(getRefreshEntriesValueChangeListener());
        showEvents.addValueChangeListener(getRefreshEntriesValueChangeListener());
        showOutlookEvents.addValueChangeListener(getRefreshEntriesValueChangeListener());
        showWorkReport.addValueChangeListener(getRefreshEntriesValueChangeListener());
    }

    private HasValue.ValueChangeListener<
            ? super AbstractField.ComponentValueChangeEvent<Checkbox, Boolean>
            > getRefreshEntriesValueChangeListener() {
        return valueChangeEvent -> refreshCalendar();
    }

    private void refreshEntries(LazyInMemoryEntryProvider<Entry> provider) {
        provider.removeAllEntries();
        List<Entry> entryList = new ArrayList<>();
        if (Boolean.TRUE.equals(showProjects.getValue())) {
            entryList.addAll(getProjectEntryList());
        }
        if (Boolean.TRUE.equals(showInvoices.getValue())) {
            entryList.addAll(getInvoiceEntryList());
        }
        if (Boolean.TRUE.equals(showHolidays.getValue())) {
            entryList.addAll(getHolidayEntryList());
        }
        if (Boolean.TRUE.equals(showEvents.getValue())) {
            entryList.addAll(getEventEntryList());
        }
        if (Boolean.TRUE.equals(showOutlookEvents.getValue())) {
            entryList.addAll(getOutlookEventEntryList());
        }
        if (Boolean.TRUE.equals(showWorkReport.getValue())) {
            entryList.addAll(getWorkReportList());
        }
        provider.addEntries(entryList);
        provider.refreshAll();
    }

    private LazyInMemoryEntryProvider<Entry> getEntryProvider() {
        List<Entry> entryList = new ArrayList<>();
        if (Boolean.TRUE.equals(showProjects.getValue())) {
            entryList.addAll(getProjectEntryList());
        }
        if (Boolean.TRUE.equals(showInvoices.getValue())) {
            entryList.addAll(getInvoiceEntryList());
        }
        if (Boolean.TRUE.equals(showHolidays.getValue())) {
            entryList.addAll(getHolidayEntryList());
        }
        if (Boolean.TRUE.equals(showEvents.getValue())) {
            entryList.addAll(getEventEntryList());
        }
        if (Boolean.TRUE.equals(showOutlookEvents.getValue())) {
            entryList.addAll(getOutlookEventEntryList());
        }
        if (Boolean.TRUE.equals(showWorkReport.getValue())) {
            entryList.addAll(getWorkReportList());
        }
        return EntryProvider.lazyInMemoryFromItems(entryList);
    }

    private String getContractName(String contractId) {
        if (contractMap.containsKey(contractId)) {
            return contractMap.get(contractId).getName();
        }
        return contractId;
    }
}
