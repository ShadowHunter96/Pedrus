package cz.bbn.cerberus.attendance.ui.component;

import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.attendance.dto.AttendanceDto;
import cz.bbn.cerberus.commons.AppInfiniteListGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.translation.Transl;


public class AttendanceGridComponent extends AppInfiniteListGrid<AttendanceDto> {

    public AttendanceGridComponent(AppEnv appEnv, ListAction<AttendanceDto> itemsAction) {
        super(appEnv, itemsAction);
        initComponent();
    }

    private void initComponent() {
        addColumn(AttendanceDto::getEmployeeName).setSortable(true)
                .setHeader(Transl.get("Employee"))
                .setComparator(AttendanceDto::getAcronym).setFrozen(true);
        addColumn(new ComponentRenderer<>(this::getHours)).setSortable(true).setHeader(Transl.get("Hours"))
                .setComparator(AttendanceDto::getHours);
        addColumn(new ComponentRenderer<>(dto ->
                getWorkDays(dto.getWork()))).setSortable(true)
                .setHeader(Transl.get("Work(D)"))
                .setComparator(AttendanceDto::getWork);
        addColumn(new ComponentRenderer<>(dto ->
                VaadinComponents.getCenteredNumberColumn(dto.getHoliday()))).setSortable(true)
                .setHeader(Transl.get(ApprovementType.HOLIDAY.name().concat("(D)")))
                .setComparator(AttendanceDto::getHoliday);
        addColumn(new ComponentRenderer<>(dto ->
                VaadinComponents.getCenteredNumberColumn(dto.getIll()))).setSortable(true)
                .setHeader(Transl.get(ApprovementType.ILL.name().concat("(D)"))).setKey("ill")
                .setComparator(AttendanceDto::getIll);
        addColumn(new ComponentRenderer<>(dto ->
                VaadinComponents.getCenteredNumberColumn(dto.getPaidLeave()))).setSortable(true)
                .setHeader(Transl.get(ApprovementType.PAID_LEAVE.name().concat("(D)")))
                .setComparator(AttendanceDto::getPaidLeave);
        addColumn(new ComponentRenderer<>(dto ->
                VaadinComponents.getCenteredNumberColumn(dto.getUnpaidLeave()))).setSortable(true)
                .setHeader(Transl.get(ApprovementType.UNPAID_LEAVE.name().concat("(D)")))
                .setComparator(AttendanceDto::getUnpaidLeave);
        addColumn(new ComponentRenderer<>(dto ->
                VaadinComponents.getCenteredNumberColumn(dto.getHomeOffice()))).setSortable(true)
                .setHeader(Transl.get(ApprovementType.HOME_OFFICE.name().concat("(D)")))
                .setComparator(AttendanceDto::getHomeOffice);
        addColumn(new ComponentRenderer<>(dto ->
                VaadinComponents.getCenteredNumberColumn(dto.getBusinessTrip()))).setSortable(true)
                .setHeader(Transl.get(ApprovementType.BUSSINES_TRIP.name()).concat("(D)"))
                .setComparator(AttendanceDto::getBusinessTrip);
        addColumn(new ComponentRenderer<>(this::getFoodVoucher)).setSortable(true).setHeader(Transl.get("Food voucher"))
                .setComparator(AttendanceDto::getFoodVouchers);
    }

    public static HorizontalLayout getWorkDays(Double number) {
        return  number == null ?
                VaadinComponents.getCenteredNumberColumn(0) : VaadinComponents.getCenteredNumberColumn(number / 8);
    }


    private HorizontalLayout getHours(AttendanceDto dto) {
        String text = ((dto.getHours() % 1) == 0 ? String.valueOf(dto.getHours().intValue())
                : dto.getHours()) + " / " + dto.getWorkDays().intValue() * 8;
        String color = "green";
        if (dto.getHours() < dto.getWorkDays() * 8) {
            color = "red";
        }
        HorizontalLayout layout = VaadinComponents.getCenteredColumn(text);
        layout.getElement().getStyle().set("color", color);
        return layout;
    }

    private HorizontalLayout getFoodVoucher(AttendanceDto dto) {
        Integer foodVoucher = dto.getFoodVouchers();
        return VaadinComponents.getCenteredNumberColumn(foodVoucher < 0 ? 0 : foodVoucher);
    }
}
