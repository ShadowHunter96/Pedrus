package cz.bbn.cerberus.attendance.ui;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.attendance.AttendanceComponentOperation;
import cz.bbn.cerberus.attendance.ui.component.AttendanceTabsComponent;
import cz.bbn.cerberus.attendance.ui.component.tab.AttendanceDocumentTabComponent;
import cz.bbn.cerberus.attendance.ui.component.tab.AttendanceTabComponent;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.holiday.HolidayService;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;

import java.util.ArrayList;
import java.util.List;

@Route(value = AttendanceView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.ATTENDANCE_VIEW)
public class AttendanceView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "attendance-list";

    private final AttendanceComponentOperation attendanceComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final HolidayService holidayService;
    private final ListService listService;

    public AttendanceView(AttendanceComponentOperation attendanceComponentOperation, AppEnv appEnv,
                          EntityNewComponentOperation entityNewComponentOperation, HolidayService holidayService,
                          ListService listService) {
        this.attendanceComponentOperation = attendanceComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.holidayService = holidayService;
        this.listService = listService;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();

        List<TabEntry> tabList = new ArrayList<>();

        ComboBox<YearMonthDto> yearMonthComboBox = new ComboBox<>(Transl.get("Month"));
        AttendanceTabComponent attendanceTabComponent = new AttendanceTabComponent(params, attendanceComponentOperation, appEnv,
                getHistoryBreadcrumbs(), holidayService.findAll(), yearMonthComboBox);
        tabList.add(new TabEntry(Transl.get("Attendance"), attendanceTabComponent));

        AttendanceDocumentTabComponent attendanceDocumentTabComponent = new AttendanceDocumentTabComponent(params, attendanceComponentOperation,
                appEnv, getHistoryBreadcrumbs(), listService);
        tabList.add(new TabEntry(Transl.get("Documents"), attendanceDocumentTabComponent, Permission.ATTENDANCE_DOCUMENT_VIEW));

        AttendanceTabsComponent attendanceTabsComponent = new AttendanceTabsComponent(Transl.get("Attendance"), tabList,
                AttendanceTabComponent.TAB_INDEX, entityNewComponentOperation, yearMonthComboBox);
        attendanceTabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.BACKOFFICE));
        this.add(attendanceTabsComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
