package cz.bbn.cerberus.attendance.ui.component.tab;

import cz.bbn.cerberus.attendance.AttendanceComponentOperation;
import cz.bbn.cerberus.attendance.ui.component.AttendanceDocumentFilterComponent;
import cz.bbn.cerberus.attendance.ui.component.AttendanceDocumentGridComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.interfaces.FilterAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.listconfiguration.ListService;

public class AttendanceDocumentTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 1;

    private final String params;
    private final AttendanceComponentOperation attendanceComponentOperation;
    private final AppEnv appEnv;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final ListService listService;

    private AttendanceDocumentGridComponent grid;

    public AttendanceDocumentTabComponent(String params, AttendanceComponentOperation attendanceComponentOperation,
                                          AppEnv appEnv, HistoryBreadcrumbs historyBreadcrumbs, ListService listService) {
        this.params = params;
        this.attendanceComponentOperation = attendanceComponentOperation;
        this.appEnv = appEnv;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.listService = listService;
        initComponent(params);
    }

    private void initComponent(String params) {
        setSizeFull();
        setId(RobotFrameworkVariables.ATTENDANCE_DOCUMENT_TAB_CARD_ID.getValue());

        AttendanceDocumentFilterComponent attendanceDocumentFilterComponent =
                new AttendanceDocumentFilterComponent(getFilterAction(), params, historyBreadcrumbs, listService.getUserDtoList());
        this.add(attendanceDocumentFilterComponent);
        grid = new AttendanceDocumentGridComponent(attendanceComponentOperation.getDeleteAction(), appEnv,
                attendanceComponentOperation.getListAction(attendanceDocumentFilterComponent), attendanceComponentOperation);
        this.add(grid);
        getFilterAction().filter();

    }

    private FilterAction getFilterAction() {
        return () -> grid.loadData();
    }

}
