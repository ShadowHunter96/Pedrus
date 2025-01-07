package cz.bbn.cerberus.attendance.ui.component.tab;

import com.vaadin.flow.component.combobox.ComboBox;
import cz.bbn.cerberus.attendance.AttendanceComponentOperation;
import cz.bbn.cerberus.attendance.ui.component.AttendanceFilterComponent;
import cz.bbn.cerberus.attendance.ui.component.AttendanceGridComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.interfaces.FilterAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;

import java.util.List;

public class AttendanceTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 0;

    private String params;
    private final AttendanceComponentOperation attendanceComponentOperation;
    private final AppEnv appEnv;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final List<HolidayEntity> holidayEntityList;
    private final ComboBox<YearMonthDto> yearMonthComboBox;

    private AttendanceGridComponent grid;

    public AttendanceTabComponent(String params, AttendanceComponentOperation attendanceComponentOperation,
                                  AppEnv appEnv, HistoryBreadcrumbs historyBreadcrumbs, List<HolidayEntity> holidayEntityList,
                                  ComboBox<YearMonthDto> yearMonthComboBox) {
        this.params = params;
        this.attendanceComponentOperation = attendanceComponentOperation;
        this.appEnv = appEnv;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.holidayEntityList = holidayEntityList;
        this.yearMonthComboBox = yearMonthComboBox;
        initComponent();
    }

    private void initComponent(){
        setSizeFull();
        setId(RobotFrameworkVariables.ATTENDANCE_TAB_CARD_ID.getValue());

        AttendanceFilterComponent attendanceFilterComponent =
                new AttendanceFilterComponent(getFilterAction(), params, historyBreadcrumbs, yearMonthComboBox);
        this.add(attendanceFilterComponent);
        grid = new AttendanceGridComponent(appEnv,
                attendanceComponentOperation.getListAction(attendanceFilterComponent, holidayEntityList));
        this.add(grid);
        getFilterAction().filter();
    }

    private FilterAction getFilterAction(){
        return () -> {
            grid.loadData();
        };
    }
}


