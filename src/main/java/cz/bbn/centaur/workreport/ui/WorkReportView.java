package cz.bbn.cerberus.workreport.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.StreamResource;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCard;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.dialog.ConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ConfirmAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.WorkReportComponentOperation;
import cz.bbn.cerberus.workreport.dto.DayWorkReportDto;
import cz.bbn.cerberus.workreport.dto.ProjectPhaseActivityDto;
import cz.bbn.cerberus.workreport.dto.WorkReportClosedDto;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;
import cz.bbn.cerberus.workreport.dto.WorkReportPickDto;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;
import cz.bbn.cerberus.workreport.ui.component.WorkReportDataComponent;
import cz.bbn.cerberus.workreport.ui.component.WorkReportDialog;
import cz.bbn.cerberus.workreport.ui.component.WorkReportGridComponent;
import cz.bbn.cerberus.workreport.ui.component.WorkReportHeaderComponent;
import cz.bbn.cerberus.workreport.ui.component.WorkReportItemsComponent;
import cz.bbn.cerberus.workreport.ui.component.WorkReportListener;
import cz.bbn.cerberus.workreport.ui.component.WorkReportSaveListener;
import lombok.extern.slf4j.Slf4j;
import org.vaadin.olli.FileDownloadWrapper;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

@Route(value = WorkReportView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.WORK_REPORT_VIEW)
@Slf4j
public class WorkReportView extends AppView implements WorkReportListener, WorkReportSaveListener {

    public static final String ROUTE = "work-report";

    private final WorkReportComponentOperation componentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final HolidayService holidayService;
    private final EmployeeDto currentEmployee;
    private final AppEnv appEnv;

    private final Button closeWorkReport =
            VaadinComponents.getButton(Transl.get("Close work report"), VaadinIcon.HOURGLASS.create());

    private WorkReportDataComponent workReportDataComponent;
    private WorkReportGridComponent workReportGridComponent;
    private WorkReportHeaderComponent workReportHeaderComponent;
    private ProjectPhaseActivityDto projectPhaseActivityDto;
    private List<WorkReportClosedDto> workReportClosedDtoList;
    private YearMonthDto yearMonthDto;
    private Div workReportDiv;

    private Binder<WorkReportDto> binder;

    public WorkReportView(WorkReportComponentOperation componentOperation,
                          EntityNewComponentOperation entityNewComponentOperation,
                          HolidayService holidayService, AppEnv appEnv) {
        this.componentOperation = componentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.currentEmployee = componentOperation.getCurrentEmployee();
        this.holidayService = holidayService;
        this.appEnv = appEnv;
        initView();
    }

    private void initView() {
        this.setSizeFull();

        this.workReportClosedDtoList = componentOperation.getClosedReports(currentEmployee);

        LocalDate now = LocalDate.now();

        binder = new Binder<>();
        WorkReportDto dto = new WorkReportDto();
        dto.setDate(now);
        binder.setBean(dto);

        workReportHeaderComponent = new WorkReportHeaderComponent(this);

        AppCard card = new AppCard(entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.setSizeFull();
        card.addToHeader(workReportHeaderComponent);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(this.entityNewComponentOperation));

        projectPhaseActivityDto = componentOperation.getProjectPhaseActivityDto(currentEmployee);

        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.setPadding(false);
        horizontalLayout.setMargin(false);
        WorkReportItemsComponent workReportItemsComponent = new WorkReportItemsComponent(this, projectPhaseActivityDto);
        horizontalLayout.add(workReportItemsComponent);
        horizontalLayout.setSizeFull();

        VerticalLayout verticalLayout = new VerticalLayout();
        workReportDataComponent = new WorkReportDataComponent(projectPhaseActivityDto,
                componentOperation.getDurationValues(), binder, this, dto, true, true, holidayService.findAll());

        workReportDiv = new Div();
        workReportDiv.setWidthFull();
        workReportDiv.add(workReportDataComponent);
        verticalLayout.add(workReportDiv);

        List<DayWorkReportDto> dayWorkReportDtoList = componentOperation.getDayWorkReport(
                AppUtils.getMonthStart(now), AppUtils.getMonthEnd(now), currentEmployee);
        workReportGridComponent = new WorkReportGridComponent(dayWorkReportDtoList, this, appEnv);
        verticalLayout.add(workReportGridComponent);
        verticalLayout.getElement().getStyle().set("padding-left", "0px");
        verticalLayout.setWidthFull();
        horizontalLayout.add(verticalLayout);

        setTotalHour(dayWorkReportDtoList, now);

        card.add(horizontalLayout);

        card.showFooter(true);

        yearMonthDto = new YearMonthDto(now.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH),
                String.valueOf(now.getYear()), now);

        if (currentEmployee != null) {
            Button pdfButton = VaadinComponents.getButton(Transl.get("Generate"), VaadinIcon.FILE_TEXT.create());
            StreamResource pdfResource = new StreamResource(
                    Transl.get("Work report").concat(" - ").concat(Transl.get(yearMonthDto.getMonth())).concat("-")
                            .concat(yearMonthDto.getYear()).concat(".pdf"), () -> getPdfFileDto().getFileData());

            FileDownloadWrapper buttonPdfWrapper = new FileDownloadWrapper(pdfResource);
            buttonPdfWrapper.wrapComponent(pdfButton);

            card.addToFooter(buttonPdfWrapper);

            card.addToFooter(closeWorkReport);

            closeWorkReport.addClickListener(e -> {
                ConfirmAction confirmAction = () -> {
                    componentOperation.closeWorkReport(yearMonthDto, currentEmployee);
                    this.workReportClosedDtoList = componentOperation.getClosedReports(currentEmployee);
                    enableDisableEditing(yearMonthDto);
                };
                ConfirmDialog confirmDialog =
                        new ConfirmDialog(Transl.get("Are you sure you want to close work report for {0} {1}",
                                Transl.get(yearMonthDto.getMonth()), yearMonthDto.getYear()), confirmAction);
                confirmDialog.open();
            });
        }

        enableDisableEditing(yearMonthDto);

        this.add(card);
    }

    private DocumentFileDto getPdfFileDto() {
        try {
            if (currentEmployee != null) {
                List<DayWorkReportDto> dayWorkReportDtoList = componentOperation.getDayWorkReport(
                        AppUtils.getMonthStart(yearMonthDto.getDate()), AppUtils.getMonthEnd(yearMonthDto.getDate()),
                        currentEmployee);
                ByteArrayOutputStream outputStream = componentOperation.getAssetPdf(dayWorkReportDtoList,
                        componentOperation.getCurrentUserName(currentEmployee), yearMonthDto.getMonth(),
                        yearMonthDto.getYear());
                DocumentFileDto documentFileDto = new DocumentFileDto();
                documentFileDto.setFileData(new ByteArrayInputStream(outputStream.toByteArray()));
                return documentFileDto;
            }
        } catch (IOException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
        return null;
    }

    private void enableDisableEditing(YearMonthDto yearMonthDto) {
        List<String> closedStringList = new ArrayList<>();
        for (WorkReportClosedDto workReportClosedDto : workReportClosedDtoList) {
            closedStringList.add(workReportClosedDto.getYearMonth());
        }
        if (!closedStringList.contains(yearMonthDto.getMonth() + yearMonthDto.getYear()) && currentEmployee != null) {
            workReportDataComponent.setComponentEnabled(true);
            workReportGridComponent.setEnableActions(true);
            closeWorkReport.setEnabled(true);
            closeWorkReport.setVisible(true);
        } else {
            workReportDataComponent.setComponentEnabled(false);
            workReportGridComponent.setEnableActions(false);
            closeWorkReport.setEnabled(false);
            closeWorkReport.setVisible(false);
        }
    }

    private void setTotalHour(List<DayWorkReportDto> dayReportList, LocalDate date) {

        double total = 0D;
        for (DayWorkReportDto report : dayReportList) {
            total = total + report.getHoursTotal();
        }

        LocalDate start = AppUtils.getMonthStart(date);
        LocalDate end = AppUtils.getMonthEnd(date);
        double required = AppUtils.countWorkDays(start, end, holidayService.findAll()) * 8;
        String color = "green";
        if (total < required) {
            color = "red";
        }
        workReportHeaderComponent.setHourText(total + "/" + required, color);
    }

    @Override
    public void changeYearMonthValue(YearMonthDto yearMonthDto) {
        this.binder = new Binder<>();
        WorkReportDto workReportDto = new WorkReportDto();
        workReportDto.setDate(yearMonthDto.getDate());
        binder.setBean(workReportDto);
        workReportDataComponent = new WorkReportDataComponent(
                projectPhaseActivityDto, componentOperation.getDurationValues(), this.binder,
                this, workReportDto, true, true, holidayService.findAll());
        workReportDiv.removeAll();
        workReportDiv.add(workReportDataComponent);
        this.yearMonthDto = yearMonthDto;
        List<DayWorkReportDto> dayReportList = componentOperation.getDayWorkReport(
                AppUtils.getMonthStart(yearMonthDto.getDate()), AppUtils.getMonthEnd(yearMonthDto.getDate()),
                currentEmployee
        );
        workReportGridComponent.setItems(dayReportList);
        setTotalHour(dayReportList, yearMonthDto.getDate());
        enableDisableEditing(yearMonthDto);
    }

    @Override
    public void changeProjectAndPhase(WorkReportPickDto pickDto, PhaseDto phase) {
        workReportDataComponent.setProjectPhase(pickDto, phase);
    }

    @Override
    public void save(Binder<WorkReportDto> binder, AppDialog dialog, WorkReportDto originalDto, boolean showDialog,
                     List<HolidayEntity> holidayEntityList) {
        if (binder.validate().isOk()) {
            LocalDate date = binder.getBean().getDate();
            if(showDialog && dialog == null && (date.getDayOfWeek().equals(DayOfWeek.SUNDAY) || date.getDayOfWeek().equals(DayOfWeek.SATURDAY)
                    || holidayEntityList.contains(new HolidayEntity(date)))) {
                ConfirmDialog confirmDialog = new ConfirmDialog(Transl.get("You are reporting on a weekend or holiday. Are you sure you want to continue?"),
                        getConfirmAction(binder, null, originalDto, false, holidayEntityList));
                confirmDialog.open();
            }else {
                WorkReportDto workReportDto = binder.getBean();
                LocalDate currentDay = workReportDto.getDate();
                componentOperation.getSaveAction(currentEmployee).saveItem(workReportDto, originalDto);
                List<DayWorkReportDto> dayReportList = componentOperation.getDayWorkReport(
                        AppUtils.getMonthStart(currentDay), AppUtils.getMonthEnd(currentDay), currentEmployee);
                workReportGridComponent.setItems(dayReportList);
                setTotalHour(dayReportList, binder.getBean().getDate());
                if (dialog != null) {
                    dialog.close();
                }
            }
        }
    }

    private ConfirmAction getConfirmAction(Binder<WorkReportDto> binder, AppDialog dialog, WorkReportDto originalDto, boolean showDialog,
                                           List<HolidayEntity> holidayEntityList){
        return () -> this.save(binder, dialog, originalDto, showDialog, holidayEntityList);
    }

    @Override
    public ConfirmAction getConfirmAction(WorkReportDto dto) {
        return () -> {
            componentOperation.deleteWorkReport(dto.getId());
            List<DayWorkReportDto> dayReportList = componentOperation.getDayWorkReport(
                    AppUtils.getMonthStart(dto.getDate()), AppUtils.getMonthEnd(dto.getDate()), currentEmployee);
            workReportGridComponent.setItems(dayReportList);
            setTotalHour(dayReportList, dto.getDate());
        };
    }

    @Override
    public void openEditDialog(WorkReportDto dto, boolean enableActions) {
        WorkReportDialog workReportDialog = new WorkReportDialog(dto, projectPhaseActivityDto,
                componentOperation.getDurationValues(), this, enableActions, true);
        workReportDialog.open();
    }
}
