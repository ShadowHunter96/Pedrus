package cz.bbn.cerberus.attendance.ui.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.listbox.ListBox;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.textfield.TextFieldVariant;
import cz.bbn.cerberus.attendance.dto.AttendanceReportDto;
import cz.bbn.cerberus.attendance.dto.AttendanceReportSummaryDto;
import cz.bbn.cerberus.attendance.dto.AttendenceEnum;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.AppHelp;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.time.format.TextStyle;
import java.util.Locale;
import java.util.Map;

public class AttendanceReportGridComponent extends HorizontalLayout {

    private static final String HEIGHT = "3em";

    private final Map<String, AttendanceReportSummaryDto> map;
    private final VerticalLayout parentContent;

    private VerticalLayout employeeLayout = new VerticalLayout();
    private VerticalLayout dayLayout = new VerticalLayout();

    public AttendanceReportGridComponent(Map<String, AttendanceReportSummaryDto> map, VerticalLayout parentContent) {
        this.map = map;
        this.parentContent = parentContent;
        initComponent();
    }

    private void initComponent() {
        addHeader(map);
        generateDataGrid();
        dayLayout.setSpacing(false);
        employeeLayout.setSpacing(false);
        dayLayout.setPadding(false);
        employeeLayout.setPadding(false);
        dayLayout.getElement().getStyle().set("padding-right", "1em");
        employeeLayout.getElement().getStyle().set("padding-left", "1em");
        employeeLayout.getElement().getStyle().set("position", "sticky").set("left", "0")
                .set("background-color", "white").set("z-index", "2");
        this.setSpacing(false);
        this.add(employeeLayout, dayLayout);
    }

    private void generateDataGrid() {
        boolean even = false;
        for (Map.Entry<String, AttendanceReportSummaryDto> entry : map.entrySet()) {
            HorizontalLayout horizontalLayout = new HorizontalLayout();
            horizontalLayout.setHeight("3em");
            horizontalLayout.setVerticalComponentAlignment(Alignment.CENTER);
            horizontalLayout.setDefaultVerticalComponentAlignment(Alignment.CENTER);
            horizontalLayout.setSpacing(false);
            Label employee = new Label();
            employee.setWidth("10em");
            employee.getElement().getStyle().set("position", "relative").set("padding", "0.6875em");
            employee.setText(entry.getValue().getAttendanceReportDtoList().stream()
                    .filter(attendanceReportDto ->
                            !StringUtils.isEmpty(attendanceReportDto.getEmployeeName())
                    ).findAny().orElse(new AttendanceReportDto(entry.getKey())).getEmployeeName());
            if (even) {
                employee.getElement().getStyle().set("background-image", "linear-gradient(#eeeeee, #eeeeee)");
            }
            horizontalLayout.add(employee);
            employeeLayout.add(horizontalLayout);
            addRows(entry.getValue(), even);
            even = !even;
        }
    }

    private void addHeader(Map<String, AttendanceReportSummaryDto> map) {
        HorizontalLayout header = new HorizontalLayout();
        Div emptyDiv = new Div();
        emptyDiv.setWidth("11.4em");
        emptyDiv.setHeight("4.5em");
        emptyDiv.setClassName("centered-day");
        emptyDiv.getElement().getStyle().set("position", "sticky").set("top", "0")
                .set("z-index", "3").set("background-color", "white");

        emptyDiv.add(generateLegendIcon());

        employeeLayout.add(emptyDiv);
        if (!map.values().isEmpty()) {
            map.values().stream().toList().get(0).getAttendanceReportDtoList().forEach(attendanceReportDto -> {
                Label dayName = new Label(Transl.get(attendanceReportDto.getDate().getDayOfWeek()
                        .getDisplayName(TextStyle.SHORT, Locale.ENGLISH)));
                Label dayNumber = new Label(AppUtils.formatDayMonth(attendanceReportDto.getDate(), AppUtils.DAY_FORMAT));
                VerticalLayout day = new VerticalLayout(dayNumber, dayName);
                day.setWidth("40px");
                day.setHeight("4.5em");
                day.setClassName("centered-day");
                header.add(day);
            });
        }
        HorizontalLayout workLayout = new HorizontalLayout();
        workLayout.getElement().getStyle().set("justify-content", "center");
        Label workDays = new Label(Transl.get("Work(D)"));
        workLayout.setWidth("10em");
        workLayout.add(workDays);

        HorizontalLayout holidayLayout = new HorizontalLayout();
        holidayLayout.getElement().getStyle().set("justify-content", "center");
        Label holidays = new Label(Transl.get("Holiday"));
        holidayLayout.setWidth("10em");
        holidayLayout.add(holidays);

        HorizontalLayout illLayout = new HorizontalLayout();
        illLayout.getElement().getStyle().set("justify-content", "center");
        Label ill = new Label(Transl.get("Ill"));
        illLayout.setWidth("10em");
        illLayout.add(ill);

        HorizontalLayout foodWoucherLayout = new HorizontalLayout();
        foodWoucherLayout.getElement().getStyle().set("justify-content", "center");
        Label foodVouchers = new Label(Transl.get("Food vouchers"));
        foodWoucherLayout.setWidth("10em");
        foodWoucherLayout.add(foodVouchers);

        HorizontalLayout summaryLayout = new HorizontalLayout();
        summaryLayout.getElement().getStyle().set("justify-content", "center");
        Label summary = new Label(Transl.get("Hours / Work hours"));
        summaryLayout.setWidth("10em");
        summaryLayout.add(summary);

        header.add(summaryLayout, workLayout, holidayLayout, illLayout, foodWoucherLayout);
        header.setMargin(false);
        header.setPadding(false);
        header.getElement().getStyle().set("position", "sticky").set("top", "0")
                .set("background-color", "white").set("z-index", "1");
        dayLayout.add(header);
    }

    private void addRows(AttendanceReportSummaryDto attendanceReportSummaryDto, boolean even) {
        HorizontalLayout rowLayout = new HorizontalLayout();
        rowLayout.setHeight("3em");
        if (even) {
            rowLayout.getElement().getStyle().set("background-image", "linear-gradient(#eeeeee, #eeeeee)");
        }
        attendanceReportSummaryDto.getAttendanceReportDtoList().forEach(attendanceReportDto -> {
            TextField value = new TextField();
            value.setReadOnly(true);
            value.setMaxLength(2);
            value.setClassName(attendanceReportDto.getAttendenceEnum().getColor());
            value.setValue(attendanceReportDto.getAttendenceEnum().getValue());
            value.setHeight("3em");
            value.setWidth("40px");
            value.getElement().getStyle().set("font-size", "12px");
            value.addThemeVariants(TextFieldVariant.LUMO_ALIGN_CENTER);
            rowLayout.add(value);
        });

        HorizontalLayout summaryLayout = new HorizontalLayout();
        summaryLayout.getElement().getStyle().set("justify-content", "center");
        summaryLayout.setAlignItems(Alignment.CENTER);
        Label summary = new Label();
        String color = "green";
        if (attendanceReportSummaryDto.getAttendanceDto().getHours() < attendanceReportSummaryDto.getAttendanceDto().getWorkDays() * 8) {
            color = "red";
        }
        summary.setText((AppUtils.doubleToString(attendanceReportSummaryDto.getAttendanceDto().getHours())) + " / " +
                (AppUtils.doubleToString(attendanceReportSummaryDto.getAttendanceDto().getWorkDays() * 8)));
        summary.getElement().getStyle().set("font-size", "12px");
        summary.getElement().getStyle().set("color", color);
        summaryLayout.setWidth("10em");
        summaryLayout.setHeight("3em");
        summaryLayout.add(summary);
        rowLayout.add(summaryLayout);

        HorizontalLayout workLayout = new HorizontalLayout();
        workLayout.getElement().getStyle().set("justify-content", "center");
        workLayout.setAlignItems(Alignment.CENTER);
        Label workDays = new Label();
        Double realWorkDays = new Double(attendanceReportSummaryDto.getAttendanceDto().getWork() == 0 ? 0 : attendanceReportSummaryDto.getAttendanceDto().getWork() / 8);
        workDays.setText(AppUtils.doubleToString(realWorkDays));
        workDays.getElement().getStyle().set("font-size", "12px");
        workLayout.setWidth("10em");
        workLayout.setHeight("3em");
        workLayout.add(workDays);
        rowLayout.add(workLayout);

        HorizontalLayout holidayLayout = new HorizontalLayout();
        holidayLayout.getElement().getStyle().set("justify-content", "center");
        holidayLayout.setAlignItems(Alignment.CENTER);
        Label holidays = new Label();
        holidays.setText(AppUtils.doubleToString(attendanceReportSummaryDto.getAttendanceDto().getHoliday()));
        holidays.getElement().getStyle().set("font-size", "12px");
        holidayLayout.setWidth("10em");
        holidayLayout.setHeight("3em");
        holidayLayout.add(holidays);
        rowLayout.add(holidayLayout);

        HorizontalLayout illLayout = new HorizontalLayout();
        illLayout.getElement().getStyle().set("justify-content", "center");
        illLayout.setAlignItems(Alignment.CENTER);
        Label ill = new Label();
        ill.setText(attendanceReportSummaryDto.getAttendanceDto().getIll().toString());
        ill.getElement().getStyle().set("font-size", "12px");
        illLayout.setWidth("10em");
        illLayout.setHeight("3em");
        illLayout.add(ill);
        rowLayout.add(illLayout);

        HorizontalLayout foodVouchersLayout = new HorizontalLayout();
        foodVouchersLayout.getElement().getStyle().set("justify-content", "center");
        foodVouchersLayout.setAlignItems(Alignment.CENTER);
        Label foodVouchers = new Label();
        foodVouchers.setText(attendanceReportSummaryDto.getAttendanceDto().getFoodVouchers().toString());
        foodVouchers.getElement().getStyle().set("font-size", "12px");
        foodVouchersLayout.setWidth("10em");
        foodVouchersLayout.add(foodVouchers);
        foodVouchersLayout.setHeight("3em");
        rowLayout.add(foodVouchersLayout);

        dayLayout.add(rowLayout);
    }

    private AppHelp generateLegendIcon() {

        AppHelp info = new AppHelp(parentContent, Transl.get("Legend").concat(":"), 1);
        info.getElement().getStyle().set("margin-top", "1.5em");
        info.setContent(getListBox());

        return info;
    }

    private ListBox<Component> getListBox() {
        ListBox<Component> listBox = new ListBox<>();
        listBox.setWidth("15em");
        listBox.setHeight(String.valueOf((3.1 * AttendenceEnum.values().length) + 2).concat("em"));

        for (AttendenceEnum attendenceEnum : AttendenceEnum.values()) {
            HorizontalLayout layout = new HorizontalLayout();
            layout.setPadding(false);
            layout.setWidth("13em");
            layout.setDefaultVerticalComponentAlignment(Alignment.CENTER);
            layout.setHeight(HEIGHT);

            TextField value = new TextField();
            value.setReadOnly(true);
            value.setMaxLength(2);
            value.setWidth("3em");
            value.setClassName(attendenceEnum.getColor());
            value.setValue(attendenceEnum.getValue());
            value.setHeight("3em");
            value.getElement().getStyle().set("font-size", "12px");
            value.addThemeVariants(TextFieldVariant.LUMO_ALIGN_CENTER);
            layout.add(value);

            Label label = new Label(" - ".concat(Transl.get(attendenceEnum.getDescription())));
            layout.add(label);
            listBox.add(layout);
        }

        return listBox;
    }
}
