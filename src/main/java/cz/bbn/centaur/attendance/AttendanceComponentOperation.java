package cz.bbn.cerberus.attendance;

import cz.bbn.cerberus.attendance.dto.AttendanceDocumentFilterDto;
import cz.bbn.cerberus.attendance.dto.AttendanceDto;
import cz.bbn.cerberus.attendance.dto.AttendanceFilterDto;
import cz.bbn.cerberus.attendance.dto.AttendanceReportSummaryDto;
import cz.bbn.cerberus.attendance.dto.AttendanceSimpleDocumentDto;
import cz.bbn.cerberus.attendance.persistance.entity.AttendanceDocumentEntity;
import cz.bbn.cerberus.attendance.ui.component.AttendanceDocumentFilterComponent;
import cz.bbn.cerberus.attendance.ui.component.AttendanceFilterComponent;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.MapAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;


@Component
@Slf4j
public class AttendanceComponentOperation {

    private final AttendanceService attendanceService;
    private final HolidayService holidayService;
    private final AppEnv appEnv;

    public AttendanceComponentOperation(AttendanceService attendanceService, HolidayService holidayService, AppEnv appEnv) {
        this.attendanceService = attendanceService;
        this.holidayService = holidayService;
        this.appEnv = appEnv;
    }

    public ListAction<AttendanceDto> getListAction(
            AttendanceFilterComponent filterComponent, List<HolidayEntity> holidayEntityList) {
        return (id) -> {
            AttendanceFilterDto filter = filterComponent.getAttendanceFilterDto();
            return attendanceService.getAttendanceDtoList(filter.getYear(), filter.getMonth(), holidayEntityList);
        };
    }

    public ItemsAction<AttendanceSimpleDocumentDto> getListAction(
            AttendanceDocumentFilterComponent filterComponent) {
        return (query, orderList) -> {
            AttendanceDocumentFilterDto filter = filterComponent.getAttendanceDocumentFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return attendanceService.findAttendanceDocumentDtoPage(filter);
        };
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                attendanceService.delete(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public ByteArrayOutputStream getAttendancePdf(Map<String, AttendanceReportSummaryDto> map, String name) throws IOException {
        ByteArrayOutputStream pdf = attendanceService.getAttendancePdf(map);
        attendanceService.saveAttendanceDocument(name, pdf.toByteArray());
        return pdf;
    }

    public ByteArrayOutputStream getAccountingPdf(Map<String, AttendanceReportSummaryDto> map, String name) throws IOException {
        ByteArrayOutputStream pdf = attendanceService.getAccountingPdf(map);
        attendanceService.saveAttendanceDocument(name, pdf.toByteArray());
        return pdf;
    }

    public ByteArrayInputStream getPdf(Long id) {
        try {
            AttendanceDocumentEntity attendanceDocumentEntity = attendanceService.getEntityById(id);
            return new ByteArrayInputStream(attendanceDocumentEntity.getFile());
        } catch (SystemException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex.getMessage(), appEnv);
        }
        return null;
    }

    public MapAction<AttendanceReportSummaryDto> getMapAction(int year, int month) {
        return () ->
                getSortedMap(attendanceService.findAttendanceReportDtoMap(year, month, holidayService.findAll()));
    }

    private Map<String, AttendanceReportSummaryDto> getSortedMap(Map<String, AttendanceReportSummaryDto> unsortedMap) {
        Map<String, AttendanceReportSummaryDto> sortedMap = new LinkedHashMap<>();
        List<String> keyList = new ArrayList<>(unsortedMap.keySet());
        Collections.sort(keyList);
        for (String key : keyList) {
            sortedMap.put(key, unsortedMap.get(key));
        }
        return sortedMap;
    }
}
