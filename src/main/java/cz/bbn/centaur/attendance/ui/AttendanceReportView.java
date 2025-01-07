package cz.bbn.cerberus.attendance.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.StreamResource;
import cz.bbn.cerberus.attendance.AttendanceComponentOperation;
import cz.bbn.cerberus.attendance.dto.AttendanceReportSummaryDto;
import cz.bbn.cerberus.attendance.ui.component.AttendanceReportGridComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.vaadin.olli.FileDownloadWrapper;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Map;

@Slf4j
@Route(value = AttendanceReportView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.ATTENDANCE_VIEW)
public class AttendanceReportView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "attendance-report-list";

    private final EntityNewComponentOperation entityNewComponentOperation;
    private final AttendanceComponentOperation attendanceComponentOperation;
    private final AppEnv appEnv;

    public AttendanceReportView(EntityNewComponentOperation entityNewComponentOperation,
                                AttendanceComponentOperation attendanceComponentOperation, AppEnv appEnv) {
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.attendanceComponentOperation = attendanceComponentOperation;
        this.appEnv = appEnv;
    }

    private void initView(String[] array) {
        removeAll();
        setSizeFull();

        Map<String, AttendanceReportSummaryDto> map = attendanceComponentOperation
                .getMapAction(Integer.valueOf(array[1]), (Integer.valueOf(array[0]))).getMap();
        AttendanceReportGridComponent grid = new AttendanceReportGridComponent(map, this);
        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Attendance report")
                .concat(" - ")
                .concat(array[1])
                .concat("/")
                .concat(array[0]), entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.setId(RobotFrameworkVariables.OPPORTUNITY_VIEW_CARD_ID.getValue());
        card.add(grid);
        card.getContent().setPadding(false);
        card.getContent().setSpacing(false);

        Button attendanceButton = VaadinComponents.getButton(
                Transl.get("Attendance report"), VaadinIcon.FILE_TEXT.create());
        String attendanceName = Transl.get("Attendance_report").concat("_").concat(array[0]).concat("_")
                .concat(array[1]).concat(".pdf");
        StreamResource attendancePdfResource = new StreamResource(
                attendanceName, () -> getAttendancePdfFileDto(map, attendanceName).getFileData());

        FileDownloadWrapper attendanceButtonPdfWrapper = new FileDownloadWrapper(attendancePdfResource);
        attendanceButtonPdfWrapper.wrapComponent(attendanceButton);
        card.addToFooter(attendanceButtonPdfWrapper);

        Button accountingButton = VaadinComponents.getButton(
                Transl.get("Accounting report"), VaadinIcon.FILE_TEXT.create());
        String accountingName = Transl.get("Accounting_report").concat("_").concat(array[0]).concat("_")
                .concat(array[1]).concat(".pdf");
        StreamResource accountingPdfResource = new StreamResource(
                accountingName, () -> getAccountingPdfFileDto(map, accountingName).getFileData());

        FileDownloadWrapper accountingPdfWrapper = new FileDownloadWrapper(accountingPdfResource);
        accountingPdfWrapper.wrapComponent(accountingButton);
        card.addToFooter(accountingPdfWrapper);

        card.showFooter(true);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(this.entityNewComponentOperation));
        add(card);
    }

    private DocumentFileDto getAttendancePdfFileDto(Map<String, AttendanceReportSummaryDto> map, String name) {
        try {
            ByteArrayOutputStream outputStream = attendanceComponentOperation.getAttendancePdf(map, name);
            DocumentFileDto documentFileDto = new DocumentFileDto();
            documentFileDto.setFileData(new ByteArrayInputStream(outputStream.toByteArray()));
            return documentFileDto;

        } catch (IOException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
        return null;
    }

    private DocumentFileDto getAccountingPdfFileDto(Map<String, AttendanceReportSummaryDto> map, String name) {
        try {
            ByteArrayOutputStream outputStream = attendanceComponentOperation.getAccountingPdf(map, name);
            DocumentFileDto documentFileDto = new DocumentFileDto();
            documentFileDto.setFileData(new ByteArrayInputStream(outputStream.toByteArray()));
            return documentFileDto;

        } catch (IOException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
        return null;
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, String params) {
        String[] array = params.replace("&month=", "").split(",");
        initView(array);
    }
}
