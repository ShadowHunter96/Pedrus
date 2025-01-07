package cz.bbn.cerberus.attendance;

import com.github.appreciated.apexcharts.config.legend.HorizontalAlign;
import com.lowagie.text.Document;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.PageSize;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfWriter;
import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementDayEntity;
import cz.bbn.cerberus.approvement.persistance.repository.ApprovementDayRepository;
import cz.bbn.cerberus.attendance.dto.AttendanceDocumentFilterDto;
import cz.bbn.cerberus.attendance.dto.AttendanceDto;
import cz.bbn.cerberus.attendance.dto.AttendanceReportDto;
import cz.bbn.cerberus.attendance.dto.AttendanceReportSummaryDto;
import cz.bbn.cerberus.attendance.dto.AttendanceSimpleDocumentDto;
import cz.bbn.cerberus.attendance.dto.AttendenceEnum;
import cz.bbn.cerberus.attendance.dto.DayDto;
import cz.bbn.cerberus.attendance.persistance.dao.AttendanceDocumentDao;
import cz.bbn.cerberus.attendance.persistance.entity.AttendanceDocumentEntity;
import cz.bbn.cerberus.attendance.persistance.entity.AttendanceReportEntity;
import cz.bbn.cerberus.attendance.persistance.repository.AttendanceDocumentRepository;
import cz.bbn.cerberus.attendance.persistance.repository.AttendanceReportRepository;
import cz.bbn.cerberus.attendance.persistance.repository.AttendanceSimpleDocumentRepository;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.WorkReportService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

@Service
@Slf4j
public class AttendanceService {

    private final AttendanceDocumentDao attendanceDocumentDao;
    private final AttendanceReportRepository attendanceReportRepository;
    private final AttendanceSimpleDocumentRepository attendanceSimpleDocumentRepository;
    private final AttendanceDocumentRepository attendanceDocumentRepository;
    private final ApprovementDayRepository approvementDayRepository;
    private final AppLogService appLogService;
    private final AppEnv appEnv;

    public AttendanceService(AttendanceDocumentDao attendanceDocumentDao,
                             AttendanceReportRepository attendanceReportRepository,
                             AttendanceSimpleDocumentRepository attendanceSimpleDocumentRepository,
                             AttendanceDocumentRepository attendanceDocumentRepository, ApprovementDayRepository approvementDayRepository, AppLogService appLogService,
                             AppEnv appEnv) {
        this.attendanceDocumentDao = attendanceDocumentDao;
        this.attendanceReportRepository = attendanceReportRepository;
        this.attendanceSimpleDocumentRepository = attendanceSimpleDocumentRepository;
        this.attendanceDocumentRepository = attendanceDocumentRepository;
        this.approvementDayRepository = approvementDayRepository;
        this.appLogService = appLogService;
        this.appEnv = appEnv;
    }

    public Page<AttendanceSimpleDocumentDto> findAttendanceDocumentDtoPage(AttendanceDocumentFilterDto filter) {
        return attendanceDocumentDao.findAttendanceDocumentDtoPage(filter);
    }

    @Transactional
    public void delete(String id) throws SystemException {
        AttendanceDocumentEntity entity = getEntityById(Long.valueOf(id));
        entity.setDeleted(Boolean.TRUE);
        attendanceDocumentRepository.save(entity);
        appLogService.logDelete(id, "ATTENDANCE_DOCUMENT");
    }

    public AttendanceDocumentEntity getEntityById(Long id) throws SystemException {
        return attendanceDocumentRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.ATTENDANCE_DOCUMENT_NOT_EXITS, id));
    }

    public Map<String, AttendanceReportSummaryDto> findAttendanceReportDtoMap(int year, int month,
                                                                              List<HolidayEntity> holidayEntityList) {
        List<AttendanceReportEntity> attendanceReportEntityList =
                attendanceReportRepository.findAttendanceReportEntitySet(year, month);
        List<DayDto> dayDtoList = generateDayDtoList(year, month, holidayEntityList);
        Map<String, AttendanceReportSummaryDto> map = new TreeMap<>();

        attendanceReportEntityList
                .stream()
                .collect(Collectors.groupingBy(attendanceReportEntity ->
                        attendanceReportEntity.getId().getEmployeeId()))
                .forEach((employeeId, attendanceReportEntities) ->
                        map.put(employeeId, new AttendanceReportSummaryDto()));

        map.forEach((employeeId, attendanceReportSummaryDto) -> {
                    dayDtoList.forEach(dayDto -> {
                                AttendanceReportDto attendanceReportDto = getAttendanceReportDto(
                                        employeeId, dayDto, attendanceReportEntityList, attendanceReportSummaryDto);
                                if (attendanceReportDto.getDate().getDayOfMonth() == 1) {
                                    attendanceReportDto.setEmployeeName(attendanceReportEntityList.stream()
                                            .filter(attendanceReportDto1 ->
                                                    attendanceReportDto1.getId().getEmployeeId().equals(employeeId))
                                            .findAny().orElse(new AttendanceReportEntity(employeeId)).getEmployeeName());
                                }
                                attendanceReportSummaryDto.getAttendanceReportDtoList().add(attendanceReportDto);
                            }
                    );
                    calculateFoodVouchers(attendanceReportSummaryDto);
                }
        );
        return map;
    }
    
    private void calculateFoodVouchers(AttendanceReportSummaryDto attendanceReportSummaryDto) {
        LocalDateTime dateFrom = LocalDateTime.now().withMonth(attendanceReportSummaryDto.getMonth())
                .withYear(attendanceReportSummaryDto.getYear()).minusMonths(1);
        LocalDateTime dateTo = LocalDate.from(dateFrom).plusMonths(1).atTime(LocalTime.MAX);

        List<ApprovementDayEntity> businessTripList =
                approvementDayRepository.getBusinessTripApprovement(dateFrom,
                        dateTo, attendanceReportSummaryDto.getUserId());

        attendanceReportSummaryDto.getAttendanceReportDtoList().forEach(attendanceReportDto -> {
            if(attendanceReportDto.getAttendenceEnum().equals(AttendenceEnum.BUSINESS_TRIP)) {
                ApprovementDayEntity approvementDayEntity = businessTripList.stream().filter(entity ->
                        entity.getDate().toLocalDate().equals(attendanceReportDto.getDate())).findAny().orElse(null);
                if(approvementDayEntity != null){
                    long count = businessTripList.stream().filter(actualDayEntity ->
                            actualDayEntity.getApprovementEntity().getId().equals(approvementDayEntity.getApprovementEntity().getId())).count();
                    if(count == 1){
                        attendanceReportSummaryDto.getAttendanceDto().plusFoodVoucher(1);
                        attendanceReportSummaryDto.plusReportFoodVouchers(1);
                    }
                }
            }
        });
    }

    public List<AttendanceDto> getAttendanceDtoList(int year, int month, List<HolidayEntity> holidayEntityList) {
        Map<String, AttendanceReportSummaryDto> map = findAttendanceReportDtoMap(year, month, holidayEntityList);
        List<AttendanceDto> attendanceDtoList = new ArrayList<>();
        map.forEach((s, attendanceReportSummaryDto) -> {
            AttendanceDto attendanceDto = attendanceReportSummaryDto.getAttendanceDto();
            attendanceDto.setEmployeeName(attendanceReportSummaryDto.getEmployeeName());
            attendanceDto.setAcronym(attendanceDto.getAcronym());
            attendanceDtoList.add(attendanceDto);
        });
        return attendanceDtoList;
    }

    private AttendanceReportDto getAttendanceReportDto(String employeeId, DayDto dayDto,
                                                       List<AttendanceReportEntity> attendanceReportEntityList,
                                                       AttendanceReportSummaryDto attendanceReportSummaryDto) {
        AttendanceReportDto attendanceReportDto = new AttendanceReportDto();

        AttendanceReportEntity attendanceReportEntity = attendanceReportEntityList.stream()
                .filter(entity -> entity.getId().getEmployeeId().equals(employeeId)
                        && entity.getId().getDate().equals(dayDto.getDate())).findAny().orElse(null);

        attendanceReportDto.setDate(dayDto.getDate());
        attendanceReportDto.setEmployeeId(employeeId);
        if (attendanceReportEntity != null) {
            attendanceReportDto.setEmployeeName(attendanceReportEntity.getEmployeeName());

            if (attendanceReportSummaryDto.getEmployeeId() == null) {
                attendanceReportSummaryDto.setEmployeeId(attendanceReportDto.getEmployeeId());
                attendanceReportSummaryDto.setEmployeeName(attendanceReportDto.getEmployeeName());
                attendanceReportSummaryDto.setAcronym(attendanceReportEntity.getAcronym());
                attendanceReportSummaryDto.setUserId(attendanceReportEntity.getUserId());
                attendanceReportSummaryDto.setYear(dayDto.getDate().getYear());
                attendanceReportSummaryDto.setMonth(dayDto.getDate().getMonth().getValue());
            }
        }

        if (attendanceReportEntity == null) {
            if (dayDto.isWeekend()) {
                attendanceReportDto.setAttendenceEnum(AttendenceEnum.WEEKEND);
            } else if (dayDto.isHoliday()) {
                attendanceReportDto.setAttendenceEnum(AttendenceEnum.PUBLIC_HOLIDAY);
            } else {
                attendanceReportDto.setAttendenceEnum(AttendenceEnum.UNEMPLOYED);
            }
        } else {
            if (dayDto.isWeekend() || dayDto.isHoliday()) {
                if (dayDto.isWeekend() && attendanceReportEntity.getBusinessTrip() == 0) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.WEEKEND);
                }
                if (dayDto.isHoliday() && attendanceReportEntity.getBusinessTrip() == 0) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.PUBLIC_HOLIDAY);
                } else if (attendanceReportEntity.getBusinessTrip() > 0) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.BUSINESS_TRIP_NO_WORK_DAY);
                }
            } else {
                if (attendanceReportEntity.getPaidLeave() > 0) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.PAID_LEAVE);
                    attendanceReportSummaryDto.getAttendanceDto().plusPaidLeave(1);
                }
                if (attendanceReportEntity.getUnpaidLeave() > 0) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.UNPAID_LEAVE);
                    attendanceReportSummaryDto.getAttendanceDto().plusUnpaidLeave(1);
                }

                if (attendanceReportEntity.getWork() > 7) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.WORK_DAY);
                    attendanceReportSummaryDto.getAttendanceDto().plusWork(attendanceReportEntity.getWork());
                    attendanceReportDto.setWork(attendanceReportEntity.getWork());
                } else if (attendanceReportEntity.getWork() > 3) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.HALF_WORK_DAY);
                    attendanceReportSummaryDto.getAttendanceDto().plusWork(attendanceReportEntity.getWork());
                    attendanceReportDto.setWork(attendanceReportEntity.getWork());
                } else if (attendanceReportEntity.getWork() < 4 && isNotWorking(attendanceReportDto)) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.UNFILLED_WORK_DAY);
                    attendanceReportSummaryDto.getAttendanceDto().plusWork(attendanceReportEntity.getWork());
                    attendanceReportDto.setWork(0D);
                }

                if (attendanceReportEntity.getIll() > 0) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.ILL);
                    attendanceReportSummaryDto.getAttendanceDto().plusIll(1);
                }
                if (attendanceReportEntity.getHomeOffice() > 0) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.HOME_OFFICE);
                    attendanceReportSummaryDto.getAttendanceDto().plusHomeOffice(1);
                }

                if (attendanceReportEntity.getHoliday() > 0) {
                    if (attendanceReportEntity.getHoliday() == 0.5) {
                        attendanceReportDto.setAttendenceEnum(AttendenceEnum.HALF_HOLIDAY);
                        attendanceReportSummaryDto.getAttendanceDto().plusHoliday(0.5);
                    } else {
                        attendanceReportDto.setAttendenceEnum(AttendenceEnum.HOLIDAY);
                        attendanceReportSummaryDto.getAttendanceDto().plusHoliday(1D);
                    }
                }

                if (attendanceReportEntity.getBusinessTrip() > 0) {
                    attendanceReportDto.setAttendenceEnum(AttendenceEnum.BUSINESS_TRIP);
                    attendanceReportSummaryDto.getAttendanceDto().plusBusinessTrip(1);
                }

                if (isNotWorking(attendanceReportDto)) {

                    if (attendanceReportDto.getAttendenceEnum().equals(AttendenceEnum.HALF_HOLIDAY)) {
                        attendanceReportSummaryDto.getAttendanceDto().plusHours(4D);
                        attendanceReportSummaryDto.plusReportWork(0.5);
                    } else {
                        attendanceReportSummaryDto.plusReportWork(1D);
                    }

                    if (attendanceReportEntity.getWork() > 0) {
                        attendanceReportSummaryDto.getAttendanceDto().plusHours(attendanceReportEntity.getWork());
                    }
                    if (attendanceReportEntity.getWork() > 3
                            && !attendanceReportDto.getAttendenceEnum().equals(AttendenceEnum.HOME_OFFICE)
                            && !attendanceReportDto.getAttendenceEnum().equals(AttendenceEnum.BUSINESS_TRIP)) {
                        attendanceReportDto.setFoodVoucher(true);
                        attendanceReportSummaryDto.getAttendanceDto().plusFoodVoucher(1);
                    }

                    // pro business trip dopocitavam stravenky az pozdeji
                    if (!attendanceReportDto.getAttendenceEnum().equals(AttendenceEnum.HOME_OFFICE)
                            && !attendanceReportDto.getAttendenceEnum().equals(AttendenceEnum.BUSINESS_TRIP)) {
                        attendanceReportDto.setReportFoodVoucher(true);
                        attendanceReportSummaryDto.plusReportFoodVouchers(1);
                    }
                } else {
                    attendanceReportSummaryDto.getAttendanceDto().plusHours(8D);
                }

                if (attendanceReportDto.getDate().isEqual(attendanceReportDto.getDate())
                        && attendanceReportDto.getDate().isBefore(attendanceReportDto.getDate())) {
                    attendanceReportSummaryDto.plusWorkDaysFoodVouchers(1);
                }
                attendanceReportSummaryDto.getAttendanceDto().plusWorkDays(1);
                attendanceReportSummaryDto.setReportDays(attendanceReportSummaryDto.getReportDays() + 1);
            }
        }
        return attendanceReportDto;
    }

    private boolean isNotWorking(AttendanceReportDto attendanceReportDto){
        return attendanceReportDto.getAttendenceEnum() != AttendenceEnum.ILL
                && attendanceReportDto.getAttendenceEnum() != AttendenceEnum.HOLIDAY
                && attendanceReportDto.getAttendenceEnum() != AttendenceEnum.PAID_LEAVE
                && attendanceReportDto.getAttendenceEnum() != AttendenceEnum.UNPAID_LEAVE;
    }

    public ByteArrayOutputStream getAttendancePdf(Map<String, AttendanceReportSummaryDto> map) throws IOException {
        ByteArrayOutputStream fos = new ByteArrayOutputStream();
        Document document = new Document(PageSize.A4.rotate(), 20, 20, 30, 20);
        PdfWriter writer = PdfWriter.getInstance(document, fos);

        try {
            BaseFont footerHeaderFont = BaseFont.createFont(BaseFont.HELVETICA, WorkReportService.ENCODING, false);
            Font headerFont = new Font(footerHeaderFont);

            HeaderFooter footer = new HeaderFooter(
                    new Phrase(Transl.get("Page").concat(": "), headerFont), true);
            footer.setBorder(Rectangle.NO_BORDER);
            footer.setAlignment(Element.ALIGN_CENTER);
            document.setFooter(footer);

            AttendanceReportSummaryDto attendanceReportSummaryDto = map.values().stream().toList().get(0);
            HeaderFooter header = new HeaderFooter(new Phrase(Transl.get("Attendance report").concat(" ")
                    .concat(String.valueOf(attendanceReportSummaryDto.getAttendanceReportDtoList()
                            .get(0).getDate().getYear())).concat("/")
                    .concat(String.valueOf(attendanceReportSummaryDto.getAttendanceReportDtoList()
                            .get(0).getDate().getMonth().getValue()))), false);
            header.setAlignment(Element.ALIGN_CENTER);
            document.setHeader(header);

            Font tableFont = FontFactory.getFont(BaseFont.HELVETICA, WorkReportService.ENCODING, 7);
            Font tableBoldFont = FontFactory.getFont(BaseFont.HELVETICA, WorkReportService.ENCODING, 7, Font.BOLD);

            document.open();

            PdfPTable table = new PdfPTable(attendanceReportSummaryDto.getAttendanceReportDtoList().size() + 6);
            table.setSpacingAfter(10f);
            List<Integer> withList = new ArrayList<>();
            withList.add(50);
            for (int i = 0; i < attendanceReportSummaryDto.getAttendanceReportDtoList().size(); i++) {
                withList.add(10);
            }
            withList.add(30);
            withList.add(30);
            withList.add(30);
            withList.add(30);
            withList.add(30);
            int[] withArr = new int[withList.size()];
            for (int i = 0; i < withList.size(); i++) {
                withArr[i] = withList.get(i);
            }
            table.setWidths(withArr);
            table.setWidthPercentage(100);

            PdfPCell name = new PdfPCell(new Phrase(Transl.get("Name"), tableBoldFont));
            name.setBackgroundColor(Color.WHITE);
            name.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(name);
            attendanceReportSummaryDto.getAttendanceReportDtoList().forEach(attendanceReportDto -> {
                PdfPCell day = new PdfPCell(new Phrase(
                        String.valueOf(attendanceReportDto.getDate().getDayOfMonth()), tableBoldFont));
                day.setBackgroundColor(Color.WHITE);
                day.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                table.addCell(day);
            });

            PdfPCell days = new PdfPCell(new Phrase(Transl.get("Work"), tableBoldFont));
            days.setBackgroundColor(Color.WHITE);
            days.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(days);

            PdfPCell holiday = new PdfPCell(new Phrase(Transl.get("Holiday"), tableBoldFont));
            holiday.setBackgroundColor(Color.WHITE);
            holiday.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(holiday);

            PdfPCell ill = new PdfPCell(new Phrase(Transl.get("Ill"), tableBoldFont));
            ill.setBackgroundColor(Color.WHITE);
            ill.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(ill);

            PdfPCell foodVoucher = new PdfPCell(new Phrase(Transl.get("Food vouchers"), tableBoldFont));
            foodVoucher.setBackgroundColor(Color.WHITE);
            foodVoucher.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(foodVoucher);

            PdfPCell wokrSummary = new PdfPCell(new Phrase(Transl.get("Summary / Work days"), tableBoldFont));
            wokrSummary.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            wokrSummary.setBackgroundColor(Color.WHITE);
            table.addCell(wokrSummary);

            map.forEach((s, dto) -> {
                PdfPCell nameCell = new PdfPCell(new Phrase(dto.getAttendanceReportDtoList()
                        .get(0).getEmployeeName(), tableBoldFont));
                nameCell.setBackgroundColor(Color.WHITE);
                table.addCell(nameCell);
                dto.getAttendanceReportDtoList().forEach(attendanceReportDto -> {
                    PdfPCell num = new PdfPCell(new Phrase(
                            attendanceReportDto.getAttendenceEnum().getValue(), tableFont));
                    num.setBackgroundColor(attendanceReportDto.getAttendenceEnum().getPdfColor());
                    num.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                    table.addCell(num);
                });

                PdfPCell workNum = new PdfPCell(new Phrase(
                        AppUtils.doubleToString(dto.getReportWork()), tableFont));
                workNum.setBackgroundColor(Color.WHITE);
                workNum.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                table.addCell(workNum);

                PdfPCell holidayNum = new PdfPCell(new Phrase(
                        AppUtils.doubleToString(dto.getAttendanceDto().getHoliday()), tableFont));
                holidayNum.setBackgroundColor(Color.WHITE);
                holidayNum.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                table.addCell(holidayNum);

                PdfPCell illNum = new PdfPCell(
                        new Phrase(String.valueOf(dto.getAttendanceDto().getIll().intValue()), tableFont));
                illNum.setBackgroundColor(Color.WHITE);
                illNum.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                table.addCell(illNum);

                PdfPCell foodVouchersNum = new PdfPCell(new Phrase(
                        String.valueOf(dto.getReportFoodVouchers()), tableFont));
                foodVouchersNum.setBackgroundColor(Color.WHITE);
                foodVouchersNum.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                table.addCell(foodVouchersNum);

                PdfPCell workSummaryRow = new PdfPCell(new Phrase(dto.getReportDays() + "/" +
                        dto.getReportDays(), tableFont));
                workSummaryRow.setBackgroundColor(Color.WHITE);
                workSummaryRow.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                table.addCell(workSummaryRow);

            });

            PdfPTable legendTable = new PdfPTable(2);
            legendTable.setHorizontalAlignment(HorizontalAlign.LEFT.ordinal());
            legendTable.setWidthPercentage(10);
            legendTable.setWidths(new int[]{10, 30});

            PdfPCell legend = new PdfPCell(new Phrase(Transl.get("legend"), tableBoldFont));
            legend.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legend.setColspan(2);
            legendTable.addCell(legend);

            PdfPCell legendWeekend = new PdfPCell(new Phrase(AttendenceEnum.WEEKEND.getValue(), tableFont));
            legendWeekend.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendWeekend.setBackgroundColor(AttendenceEnum.WEEKEND.getPdfColor());
            legendTable.addCell(legendWeekend);

            PdfPCell descriptionWeekend = new PdfPCell(
                    new Phrase(Transl.get(AttendenceEnum.WEEKEND.getDescription()), tableFont));
            legendTable.addCell(descriptionWeekend);

            PdfPCell legendPublicHoliday = new PdfPCell(
                    new Phrase(AttendenceEnum.PUBLIC_HOLIDAY.getValue(), tableFont));
            legendPublicHoliday.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendPublicHoliday.setBackgroundColor(AttendenceEnum.PUBLIC_HOLIDAY.getPdfColor());
            legendTable.addCell(legendPublicHoliday);

            PdfPCell descriptionPublicHoliday = new PdfPCell(new Phrase(
                    Transl.get(AttendenceEnum.PUBLIC_HOLIDAY.getDescription()), tableFont));
            legendTable.addCell(descriptionPublicHoliday);

            PdfPCell legendBusinessTrip = new PdfPCell(new Phrase(AttendenceEnum.BUSINESS_TRIP.getValue(), tableFont));
            legendBusinessTrip.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendBusinessTrip.setBackgroundColor(AttendenceEnum.BUSINESS_TRIP.getPdfColor());
            legendTable.addCell(legendBusinessTrip);

            PdfPCell descriptionBusinessTrip = new PdfPCell(new Phrase(
                    Transl.get(AttendenceEnum.BUSINESS_TRIP.getDescription()), tableFont));
            legendTable.addCell(descriptionBusinessTrip);

            PdfPCell legendIll = new PdfPCell(new Phrase(AttendenceEnum.ILL.getValue(), tableFont));
            legendIll.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendIll.setBackgroundColor(AttendenceEnum.ILL.getPdfColor());
            legendTable.addCell(legendIll);

            PdfPCell descriptionIll = new PdfPCell(new Phrase(
                    Transl.get(AttendenceEnum.ILL.getDescription()), tableFont));
            legendTable.addCell(descriptionIll);

            PdfPCell legendHoliday = new PdfPCell(new Phrase(AttendenceEnum.HOLIDAY.getValue(), tableFont));
            legendHoliday.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendHoliday.setBackgroundColor(AttendenceEnum.HOLIDAY.getPdfColor());
            legendTable.addCell(legendHoliday);

            PdfPCell descriptionHoliday = new PdfPCell(new Phrase(
                    Transl.get(AttendenceEnum.HOLIDAY.getDescription()), tableFont));
            legendTable.addCell(descriptionHoliday);

            PdfPCell legendHalfHoliday = new PdfPCell(new Phrase(AttendenceEnum.HALF_HOLIDAY.getValue(), tableFont));
            legendHalfHoliday.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendHalfHoliday.setBackgroundColor(AttendenceEnum.HALF_HOLIDAY.getPdfColor());
            legendTable.addCell(legendHalfHoliday);

            PdfPCell descriptionHalfHoliday = new PdfPCell(new Phrase(
                    Transl.get(AttendenceEnum.HALF_HOLIDAY.getDescription()), tableFont));
            legendTable.addCell(descriptionHalfHoliday);

            PdfPCell legendHomeOffice = new PdfPCell(new Phrase(AttendenceEnum.HOME_OFFICE.getValue(), tableFont));
            legendHomeOffice.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendHomeOffice.setBackgroundColor(AttendenceEnum.HOME_OFFICE.getPdfColor());
            legendTable.addCell(legendHomeOffice);

            PdfPCell descriptionHomeOffice = new PdfPCell(new Phrase(
                    Transl.get(AttendenceEnum.HOME_OFFICE.getDescription()), tableFont));
            legendTable.addCell(descriptionHomeOffice);

            PdfPCell legendPaidLeave = new PdfPCell(new Phrase(AttendenceEnum.PAID_LEAVE.getValue(), tableFont));
            legendPaidLeave.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendPaidLeave.setBackgroundColor(AttendenceEnum.PAID_LEAVE.getPdfColor());
            legendTable.addCell(legendPaidLeave);

            PdfPCell descriptionPaidLeave = new PdfPCell(new Phrase(
                    Transl.get(AttendenceEnum.PAID_LEAVE.getDescription()), tableFont));
            legendTable.addCell(descriptionPaidLeave);

            PdfPCell legendUnpaidLeave = new PdfPCell(new Phrase(AttendenceEnum.UNPAID_LEAVE.getValue(), tableFont));
            legendUnpaidLeave.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendUnpaidLeave.setBackgroundColor(AttendenceEnum.UNPAID_LEAVE.getPdfColor());
            legendTable.addCell(legendUnpaidLeave);

            PdfPCell descriptionUnpaidLeave = new PdfPCell(new Phrase(
                    Transl.get(AttendenceEnum.UNPAID_LEAVE.getDescription()), tableFont));
            legendTable.addCell(descriptionUnpaidLeave);

            PdfPCell legendWorkDay = new PdfPCell(new Phrase(AttendenceEnum.WORK_DAY.getValue(), tableFont));
            legendWorkDay.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendTable.addCell(legendWorkDay);

            PdfPCell descriptionWorkDay = new PdfPCell(new Phrase(
                    Transl.get(AttendenceEnum.WORK_DAY.getDescription()), tableFont));
            legendTable.addCell(descriptionWorkDay);

            PdfPCell legendUnemployed = new PdfPCell(new Phrase(AttendenceEnum.UNEMPLOYED.getValue(), tableFont));
            legendUnemployed.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            legendUnemployed.setBackgroundColor(AttendenceEnum.UNEMPLOYED.getPdfColor());
            legendTable.addCell(legendUnemployed);

            PdfPCell descriptionUnemployed = new PdfPCell(new Phrase(Transl.get(
                    AttendenceEnum.UNEMPLOYED.getDescription()), tableFont));
            legendTable.addCell(descriptionUnemployed);


            document.add(table);
            document.add(legendTable);

            return fos;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        } finally {
            document.close();
            writer.close();
        }
        return fos;
    }

    public ByteArrayOutputStream getAccountingPdf(Map<String, AttendanceReportSummaryDto> map) throws IOException {
        ByteArrayOutputStream fos = new ByteArrayOutputStream();
        Document document = new Document(PageSize.A4, 20, 20, 30, 20);
        PdfWriter writer = PdfWriter.getInstance(document, fos);

        try {
            BaseFont footerHeaderFont = BaseFont.createFont(BaseFont.HELVETICA, WorkReportService.ENCODING, false);
            Font headerFont = new Font(footerHeaderFont);

            HeaderFooter footer = new HeaderFooter(
                    new Phrase(Transl.get("Page").concat(": "), headerFont), true);
            footer.setBorder(Rectangle.NO_BORDER);
            footer.setAlignment(Element.ALIGN_CENTER);
            document.setFooter(footer);

            AttendanceReportSummaryDto attendanceReportSummaryDto = map.values().stream().toList().get(0);
            HeaderFooter header = new HeaderFooter(new Phrase(
                    Transl.get("Accounting report - food vouchers").concat(" ")
                            .concat(String.valueOf(attendanceReportSummaryDto.getAttendanceReportDtoList()
                                    .get(0).getDate().getYear())).concat("/")
                            .concat(String.valueOf(attendanceReportSummaryDto.getAttendanceReportDtoList()
                                    .get(0).getDate().getMonth().getValue()))), false);
            header.setAlignment(Element.ALIGN_CENTER);
            document.setHeader(header);

            Font tableFont = FontFactory.getFont(BaseFont.HELVETICA, WorkReportService.ENCODING, 9);
            Font tableBoldFont = FontFactory.getFont(BaseFont.HELVETICA, WorkReportService.ENCODING, 9, Font.BOLD);

            document.open();

            PdfPTable table = new PdfPTable(6);
            table.setWidths(new int[]{50, 30, 30, 30, 30, 30});
            table.setWidthPercentage(100);

            PdfPCell nameHeader = new PdfPCell(new Phrase(Transl.get("Name"), tableBoldFont));
            nameHeader.setBackgroundColor(Color.WHITE);
            nameHeader.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(nameHeader);

            PdfPCell pcsHeader = new PdfPCell(new Phrase(Transl.get("Pcs"), tableBoldFont));
            pcsHeader.setBackgroundColor(Color.WHITE);
            pcsHeader.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(pcsHeader);

            PdfPCell oneVoucherValueHeader = new PdfPCell(new Phrase(
                    Transl.get("Value of one voucher"), tableBoldFont));
            oneVoucherValueHeader.setBackgroundColor(Color.WHITE);
            oneVoucherValueHeader.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(oneVoucherValueHeader);

            PdfPCell vouchersSumValueHeader = new PdfPCell(new Phrase(Transl.get("Summary"), tableBoldFont));
            vouchersSumValueHeader.setBackgroundColor(Color.WHITE);
            vouchersSumValueHeader.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(vouchersSumValueHeader);

            PdfPCell minusEmployeeHeader = new PdfPCell(new Phrase(Transl.get("Employee deduction"), tableBoldFont));
            minusEmployeeHeader.setBackgroundColor(Color.LIGHT_GRAY);
            minusEmployeeHeader.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(minusEmployeeHeader);

            PdfPCell minusEmployerHeader = new PdfPCell(new Phrase(Transl.get("Employer deduction"), tableBoldFont));
            minusEmployerHeader.setBackgroundColor(Color.LIGHT_GRAY);
            minusEmployerHeader.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(minusEmployerHeader);

            AtomicReference<Integer> allVoucherSum = new AtomicReference<>(0);
            AtomicReference<Integer> allVoucherEmployeeSum = new AtomicReference<>(0);
            AtomicReference<Integer> allVoucherEmployerSum = new AtomicReference<>(0);
            map.forEach((s, dto) -> {
                PdfPCell nameCell = new PdfPCell(new Phrase(
                        dto.getAttendanceReportDtoList().get(0).getEmployeeName(), tableFont));
                nameCell.setBackgroundColor(Color.WHITE);
                table.addCell(nameCell);

                allVoucherSum.set(allVoucherSum.get() + dto.getReportFoodVouchers());
                PdfPCell pcs = new PdfPCell(new Phrase(String.valueOf(
                        dto.getReportFoodVouchers().intValue()), tableFont));
                pcs.setBackgroundColor(Color.WHITE);
                pcs.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                table.addCell(pcs);

                PdfPCell oneFoodVoucherValue = new PdfPCell(new Phrase(
                        appEnv.getStringProperty(AppProperty.FOOD_VOUCHER_VALUE), tableFont));
                oneFoodVoucherValue.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                oneFoodVoucherValue.setBackgroundColor(Color.WHITE);
                table.addCell(oneFoodVoucherValue);

                Integer vouchersSumValues = dto.getReportFoodVouchers() *
                        appEnv.getIntegerProperty(AppProperty.FOOD_VOUCHER_VALUE, 0);
                PdfPCell vouchersSumValue = new PdfPCell(new Phrase(
                        AppUtils.priceInteger(vouchersSumValues.intValue()), tableFont));
                vouchersSumValue.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                vouchersSumValue.setBackgroundColor(Color.WHITE);
                table.addCell(vouchersSumValue);

                Double employeeVoucherValue = Math.ceil((45.0 / 100.0) * vouchersSumValues);
                allVoucherEmployeeSum.set(allVoucherEmployeeSum.get() + employeeVoucherValue.intValue());
                PdfPCell minusEmployee = new PdfPCell(new Phrase(
                        AppUtils.priceInteger(employeeVoucherValue.intValue()), tableFont));
                minusEmployee.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                minusEmployee.setBackgroundColor(Color.LIGHT_GRAY);
                table.addCell(minusEmployee);

                Double employerVoucherValue = (55.0 / 100.0) * vouchersSumValues;
                allVoucherEmployerSum.set(allVoucherEmployerSum.get() + employerVoucherValue.intValue());
                PdfPCell minusEmployer = new PdfPCell(new Phrase(
                        AppUtils.priceInteger(employerVoucherValue.intValue()), tableFont));
                minusEmployer.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
                minusEmployer.setBackgroundColor(Color.LIGHT_GRAY);
                table.addCell(minusEmployer);
            });

            PdfPCell nameSumCell = new PdfPCell(new Phrase("", tableBoldFont));
            nameSumCell.setBackgroundColor(Color.WHITE);
            nameSumCell.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(nameSumCell);

            PdfPCell allVoucherSumCell = new PdfPCell(new Phrase(allVoucherSum.get().toString(), tableBoldFont));
            allVoucherSumCell.setBackgroundColor(Color.WHITE);
            allVoucherSumCell.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(allVoucherSumCell);

            PdfPCell emptySumCell = new PdfPCell(new Phrase("", tableBoldFont));
            emptySumCell.setBackgroundColor(Color.WHITE);
            emptySumCell.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(emptySumCell);

            PdfPCell allVoucherValueSumCell = new PdfPCell(new Phrase(
                    AppUtils.priceInteger(allVoucherEmployeeSum.get() + allVoucherEmployerSum.get()), tableBoldFont));
            allVoucherValueSumCell.setBackgroundColor(Color.WHITE);
            allVoucherValueSumCell.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(allVoucherValueSumCell);

            PdfPCell allEmployeeVoucherSumCell = new PdfPCell(new Phrase(
                    AppUtils.priceInteger(allVoucherEmployeeSum.get()), tableBoldFont));
            allEmployeeVoucherSumCell.setBackgroundColor(Color.LIGHT_GRAY);
            allEmployeeVoucherSumCell.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(allEmployeeVoucherSumCell);

            PdfPCell allEmployerVoucherSumCell = new PdfPCell(new Phrase(
                    AppUtils.priceInteger(allVoucherEmployerSum.get()), tableBoldFont));
            allEmployerVoucherSumCell.setBackgroundColor(Color.LIGHT_GRAY);
            allEmployerVoucherSumCell.setHorizontalAlignment(HorizontalAlign.CENTER.ordinal());
            table.addCell(allEmployerVoucherSumCell);

            document.add(table);
            return fos;
        } finally {
            document.close();
            writer.close();
        }
    }

    @Transactional
    public void saveAttendanceDocument(String name, byte[] file) {
        attendanceSimpleDocumentRepository.updateDeletedByName(name);
        AttendanceDocumentEntity entity = new AttendanceDocumentEntity();
        entity.setUserId(SecurityUtils.getCurrentUserId());
        entity.setName(name);
        entity.setDate(LocalDateTime.now());
        entity.setFile(file);
        entity.setDeleted(Boolean.FALSE);
        attendanceDocumentRepository.save(entity);
    }

    private List<DayDto> generateDayDtoList(int year, int month, List<HolidayEntity> holidayEntityList) {
        LocalDate startDate = AppUtils.getMonthStart(LocalDate.now().withYear(year).withMonth(month));
        LocalDate endDate = AppUtils.getMonthEnd(startDate);
        List<DayDto> dayDtoList = new ArrayList<>();
        while (startDate.isBefore(endDate) || startDate.equals(endDate)) {
            DayDto dayDto = new DayDto(startDate);
            if (holidayEntityList.contains(new HolidayEntity(startDate))) {
                dayDto.setHoliday(true);
            }
            if (startDate.getDayOfWeek().equals(DayOfWeek.SATURDAY)
                    || startDate.getDayOfWeek().equals(DayOfWeek.SUNDAY)) {
                dayDto.setWeekend(true);
            }
            dayDtoList.add(dayDto);
            startDate = startDate.plusDays(1);
        }
        return dayDtoList;
    }
}
