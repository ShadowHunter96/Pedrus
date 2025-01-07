package cz.bbn.cerberus.workreport;

import com.lowagie.text.Cell;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.PageSize;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Table;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.PdfWriter;
import com.vaadin.flow.component.datepicker.DatePicker;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.DayWorkReportDto;
import cz.bbn.cerberus.workreport.dto.WorkReportClosedDto;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;
import cz.bbn.cerberus.workreport.factory.WorkReportClosedFactory;
import cz.bbn.cerberus.workreport.factory.WorkReportFactory;
import cz.bbn.cerberus.workreport.persistance.entity.WorkReportClosedEntity;
import cz.bbn.cerberus.workreport.persistance.entity.WorkReportClosedId;
import cz.bbn.cerberus.workreport.persistance.entity.WorkReportEntity;
import cz.bbn.cerberus.workreport.persistance.repository.WorkReportClosedRepository;
import cz.bbn.cerberus.workreport.persistance.repository.WorkReportRepository;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public class WorkReportService {

    public static final String ENCODING = "Cp1250";

    private final WorkReportRepository workReportRepository;
    private final WorkReportClosedRepository workReportClosedRepository;

    public WorkReportService(WorkReportRepository workReportRepository,
                             WorkReportClosedRepository workReportClosedRepository) {
        this.workReportRepository = workReportRepository;
        this.workReportClosedRepository = workReportClosedRepository;
    }

    public void save(WorkReportDto dto) {
        WorkReportEntity entity = new WorkReportEntity();
        WorkReportFactory.fillEntity(entity, dto);
        workReportRepository.save(entity);
    }

    public List<WorkReportDto> getPeriodWorkReportDtoList(LocalDate start, LocalDate end, String empId) {
        return ConvertEntities.fromEntities(workReportRepository.getPeriodWorkReportDtoList(
                start, end, empId), WorkReportFactory::fromEntity);
    }

    public WorkReportEntity getWorkReportWithApprovement(WorkReportDto dto, EmployeeDto employeeDto) {
        return workReportRepository.getWorkReportWithApprovement(dto.getDate(), employeeDto.getId(), PageRequest.of(0, 1));
    }

    public void deleteWorkReport(Long id) {
        workReportRepository.deleteById(id);
    }

    public ByteArrayOutputStream getAssetPdf(List<DayWorkReportDto> dayWorkReportDtoList, String userName,
                                             String month, String year) throws IOException {
        ByteArrayOutputStream fos = new ByteArrayOutputStream();
        Document document = new Document(PageSize.A4, 20, 20, 30, 20);
        PdfWriter writer = PdfWriter.getInstance(document, fos);

        try {

            BaseFont footerHeaderFont = BaseFont.createFont(BaseFont.HELVETICA, ENCODING, false);
            Font headerFont = new Font(footerHeaderFont);

            HeaderFooter footer = new HeaderFooter(
                    new Phrase(Transl.get("Page").concat(": "), headerFont), true);
            footer.setBorder(Rectangle.NO_BORDER);
            footer.setAlignment(Element.ALIGN_CENTER);
            document.setFooter(footer);

            HeaderFooter header = new HeaderFooter(new Phrase(Transl.get("Work report")), false);
            header.setAlignment(Element.ALIGN_CENTER);
            document.setHeader(header);

            Font tableFont = FontFactory.getFont(BaseFont.HELVETICA, ENCODING, 9);
            Font tableBoldFont = FontFactory.getFont(BaseFont.HELVETICA, ENCODING, 9, Font.BOLD);

            document.open();

            Font chunkFont = FontFactory.getFont(BaseFont.HELVETICA, ENCODING, 11);

            Chunk userPhrase = new Chunk(userName.concat(" - ").concat(Transl.get(month))
                    .concat(" ").concat(year).concat(" - ").concat(getHoursTotal(dayWorkReportDtoList))
                    .concat("h"), chunkFont);

            Paragraph dataParagraph = new Paragraph(15);
            dataParagraph.add(userPhrase);

            document.add(dataParagraph);

            Table table = new Table(7);
            table.setBorderWidth(1);
            table.setPadding(4);
            table.setWidth(100);

            Cell totalCell = new Cell(new Phrase(Transl.get("Total"), tableBoldFont));
            totalCell.setBackgroundColor(Color.LIGHT_GRAY);
            totalCell.setWidth("10%");
            table.addCell(totalCell);

            Cell dateCell = new Cell(new Phrase(Transl.get("Date"), tableBoldFont));
            dateCell.setBackgroundColor(Color.LIGHT_GRAY);
            dateCell.setWidth("10%");
            table.addCell(dateCell);

            Cell projectCell = new Cell(new Phrase(Transl.get("Project"), tableBoldFont));
            projectCell.setBackgroundColor(Color.LIGHT_GRAY);
            projectCell.setWidth("15%");
            table.addCell(projectCell);

            Cell phaseCell = new Cell(new Phrase(Transl.get("Project phase"), tableBoldFont));
            phaseCell.setBackgroundColor(Color.LIGHT_GRAY);
            phaseCell.setWidth("15%");
            table.addCell(phaseCell);

            Cell activityCell = new Cell(new Phrase(Transl.get("Activity"), tableBoldFont));
            activityCell.setBackgroundColor(Color.LIGHT_GRAY);
            activityCell.setWidth("15%");
            table.addCell(activityCell);

            Cell hoursCell = new Cell(new Phrase(Transl.get("Hours"), tableBoldFont));
            hoursCell.setBackgroundColor(Color.LIGHT_GRAY);
            hoursCell.setWidth("10%");
            table.addCell(hoursCell);

            Cell descriptionCell = new Cell(new Phrase(Transl.get("Description"), tableBoldFont));
            descriptionCell.setBackgroundColor(Color.LIGHT_GRAY);
            descriptionCell.setWidth("25%");
            table.addCell(descriptionCell);

            for (DayWorkReportDto dayWorkReportDto : dayWorkReportDtoList) {

                Cell totalRowCell = new Cell(new Phrase(String.valueOf(dayWorkReportDto.getHoursTotal()), tableFont));
                totalRowCell.setRowspan(dayWorkReportDto.getWorkReportDtoList().size());
                totalRowCell.setWidth("10%");
                table.addCell(totalRowCell);

                Cell dateRowCell = new Cell(new Phrase(AppUtils.formatDayMonth(dayWorkReportDto.getDate()), tableFont));
                dateRowCell.setRowspan(dayWorkReportDto.getWorkReportDtoList().size());
                dateRowCell.setWidth("10%");
                table.addCell(dateRowCell);

                for (WorkReportDto workReportDto : dayWorkReportDto.getWorkReportDtoList()) {
                    Cell projectRowCell = new Cell(new Phrase(workReportDto.getPickDto() != null ?
                            workReportDto.getPickDto().getName() : "", tableFont));
                    projectRowCell.setWidth("15%");
                    table.addCell(projectRowCell);
                    Cell phaseRowCell = new Cell(new Phrase(workReportDto.getPhaseDto() != null ?
                            workReportDto.getPhaseDto().getName() : "", tableFont));
                    phaseRowCell.setWidth("15%");
                    table.addCell(phaseRowCell);
                    Cell activityRowCell = new Cell(new Phrase(workReportDto.getActivity() != null ?
                            workReportDto.getActivity().getName() : "", tableFont));
                    activityRowCell.setWidth("15%");
                    table.addCell(activityRowCell);
                    Cell durationRowCell = new Cell(new Phrase(String.valueOf(workReportDto.getDuration()), tableFont));
                    durationRowCell.setWidth("10%");
                    table.addCell(durationRowCell);
                    Cell descriptionRowCell = new Cell(new Phrase(workReportDto.getDescription(), tableFont));
                    descriptionRowCell.setWidth("25%");
                    table.addCell(descriptionRowCell);
                }
            }

            document.add(table);

            return fos;
        } finally {
            document.close();
            writer.close();
        }
    }

    public List<WorkReportClosedDto> getWorkReportClosedList(String employeeId) {
        return ConvertEntities.fromEntities(
                workReportClosedRepository.findByEmployee(employeeId), WorkReportClosedFactory::fromEntity);
    }

    public void closeWorkReport(YearMonthDto yearMonthDto, EmployeeDto employeeDto) {
        WorkReportClosedEntity entity = new WorkReportClosedEntity();
        WorkReportClosedId id = new WorkReportClosedId();
        id.setYear(yearMonthDto.getYear());
        id.setMonth(yearMonthDto.getMonth());
        id.setEmployeeId(employeeDto.getId());
        entity.setId(id);
        workReportClosedRepository.save(entity);
    }

    private String getHoursTotal(List<DayWorkReportDto> dayWorkReportDtoList) {
        Double total = 0.0;
        for (DayWorkReportDto dto : dayWorkReportDtoList) {
            total = total + dto.getHoursTotal();
        }
        return String.valueOf(total);
    }

    public String getActiveWorkReport(DatePicker from, DatePicker to, Set<String> employeeIdSet) {
        return getHoursTotalFromEntity(
                workReportRepository.getPeriodWorkReportDtoList(from.getValue(), to.getValue(), employeeIdSet));
    }

    public void deleteWorkReport(Long approvementId, String employeId) {
        workReportRepository.deleteByApprovementEntityIdAndEmployeeId(approvementId, employeId);
    }

    private String getHoursTotalFromEntity(List<WorkReportEntity> workReportDtoList) {
        Double total = 0.0;
        for (WorkReportEntity entity : workReportDtoList) {
            total = total + Optional.ofNullable(entity.getDuration()).orElse(0D);
        }
        return String.valueOf(total);
    }
}
