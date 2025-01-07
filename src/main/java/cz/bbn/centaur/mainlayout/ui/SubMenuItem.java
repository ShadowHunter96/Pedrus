package cz.bbn.cerberus.mainlayout.ui;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.administration.ui.AdministrationView;
import cz.bbn.cerberus.applog.ui.AppLogView;
import cz.bbn.cerberus.applog.ui.components.tab.AppLogTab;
import cz.bbn.cerberus.applog.ui.components.tab.SchedulerLogTab;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.approvement.ui.ApprovementApproverView;
import cz.bbn.cerberus.approvement.ui.ApprovementBusinessTripView;
import cz.bbn.cerberus.approvement.ui.ApprovementHolidayView;
import cz.bbn.cerberus.approvement.ui.ApprovementHomeOfficeView;
import cz.bbn.cerberus.approvement.ui.ApprovementIllView;
import cz.bbn.cerberus.approvement.ui.ApprovementPaidLeaveView;
import cz.bbn.cerberus.approvement.ui.ApprovementUnpaidLeaveView;
import cz.bbn.cerberus.area.ui.AreaDetailView;
import cz.bbn.cerberus.asset.ui.AssetDetailView;
import cz.bbn.cerberus.asset.ui.AssetView;
import cz.bbn.cerberus.assetposition.ui.AssetPositionDetailView;
import cz.bbn.cerberus.attendance.ui.AttendanceView;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.contactperson.ui.ContactPersonDetailView;
import cz.bbn.cerberus.contactperson.ui.ContactPersonView;
import cz.bbn.cerberus.contactpersontype.ui.ContactPersonTypeDetailView;
import cz.bbn.cerberus.contract.ui.ContractBackOfficeView;
import cz.bbn.cerberus.contract.ui.ContractSalesDetailView;
import cz.bbn.cerberus.contract.ui.ContractSalesView;
import cz.bbn.cerberus.contracttype.ui.ContractTypeDetailView;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionView;
import cz.bbn.cerberus.document.ui.DocumentView;
import cz.bbn.cerberus.documenttype.ui.DocumentTypeDetailView;
import cz.bbn.cerberus.dph.ui.DphDetailView;
import cz.bbn.cerberus.dsmessage.ui.DsMessageDetailView;
import cz.bbn.cerberus.dsmessage.ui.DsMessageView;
import cz.bbn.cerberus.dssetting.ui.DsSettingDetailView;
import cz.bbn.cerberus.email.ui.EmailView;
import cz.bbn.cerberus.employee.ui.EmployeeDetailView;
import cz.bbn.cerberus.employee.ui.EmployeeView;
import cz.bbn.cerberus.employeecontract.ui.EmployeeContractView;
import cz.bbn.cerberus.invoice.ui.InvoicingDetailView;
import cz.bbn.cerberus.invoice.ui.InvoicingView;
import cz.bbn.cerberus.label.ui.LabelDetailView;
import cz.bbn.cerberus.label.ui.LabelView;
import cz.bbn.cerberus.management.ui.ManagementDashboardView;
import cz.bbn.cerberus.management.ui.ManagementIntranetDashboardView;
import cz.bbn.cerberus.management.ui.ManagementOwnerView;
import cz.bbn.cerberus.note.ui.NoteView;
import cz.bbn.cerberus.offer.ui.OfferDetailView;
import cz.bbn.cerberus.offer.ui.OfferView;
import cz.bbn.cerberus.opportunity.ui.OpportunityDetailView;
import cz.bbn.cerberus.opportunity.ui.OpportunityView;
import cz.bbn.cerberus.permissionmanagement.ui.PermissionManagementView;
import cz.bbn.cerberus.project.ui.ProjectDetailView;
import cz.bbn.cerberus.project.ui.ProjectView;
import cz.bbn.cerberus.role.ui.RoleDetailView;
import cz.bbn.cerberus.role.ui.RoleView;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.subject.ui.SubjectView;
import cz.bbn.cerberus.suppliertype.ui.SupplierTypeDetailView;
import cz.bbn.cerberus.task.ui.TaskView;
import cz.bbn.cerberus.taskschedule.ui.TaskScheduleView;
import cz.bbn.cerberus.tasktemplate.ui.TaskTemplateView;
import cz.bbn.cerberus.technology.ui.TechnologyDetailView;
import cz.bbn.cerberus.translation.ui.TranslationView;
import cz.bbn.cerberus.user.ui.UserDetailView;
import cz.bbn.cerberus.user.ui.UserView;
import cz.bbn.cerberus.usersettings.ui.UserSettingsView;
import cz.bbn.cerberus.virtualserver.ui.VirtualServerView;
import cz.bbn.cerberus.workreport.ui.WorkReportView;

import java.util.Arrays;

public enum SubMenuItem {

    MANAGEMENT_DASHBOARD("Management dashboard", VaadinIcon.PIE_CHART, ManagementDashboardView.class, new Class[]{ManagementDashboardView.class}),
    MANAGEMENT_INTRANET_DASHBOARD("Intranet dashboard", VaadinIcon.BAR_CHART, ManagementIntranetDashboardView.class, new Class[]{ManagementIntranetDashboardView.class}),
    MANAGEMENT_OWNER("Owner management", VaadinIcon.USER_CHECK, ManagementOwnerView.class, new Class[]{ManagementOwnerView.class}),
    SUBJECT_LIST("Subject list", VaadinIcon.GOLF, SubjectView.class, new Class[]{SubjectView.class, SubjectDetailView.class}),
    OPPORTUNITY_LIST("Opportunity list", VaadinIcon.BAR_CHART_V, OpportunityView.class, new Class[]{OpportunityView.class, OpportunityDetailView.class}),
    OFFER_LIST("Offer list", VaadinIcon.DIPLOMA_SCROLL, OfferView.class, new Class[]{OfferView.class, OfferDetailView.class}),
    CONTRACT_LIST("Contract list", VaadinIcon.HANDSHAKE, ContractSalesView.class, new Class[]{ContractSalesView.class, ContractSalesDetailView.class}),
    PROJECT_LIST("Project list", VaadinIcon.GRID, ProjectView.class, new Class[]{ProjectView.class, ProjectDetailView.class}),
    CONTACT_PERSON_LIST("Contact person list", VaadinIcon.USER_HEART, ContactPersonView.class, new Class[]{ContactPersonView.class, ContactPersonDetailView.class}),
    NOTE_LIST("Note list", VaadinIcon.CLIPBOARD, NoteView.class, new Class[]{NoteView.class}),
    EMAIL_LIST("Email list", VaadinIcon.ENVELOPE_O, EmailView.class, new Class[]{EmailView.class}),

    BO_CONTRACT_LIST("BO contract list", VaadinIcon.HANDSHAKE, ContractBackOfficeView.class, new Class[]{ContractBackOfficeView.class}),
    EMP_CONTRACT_LIST("Employee contract list", VaadinIcon.BRIEFCASE, EmployeeContractView.class, new Class[]{EmployeeContractView.class}),
    ASSET_LIST("Asset list", VaadinIcon.TOOLBOX, AssetView.class, new Class[]{AssetView.class, AssetDetailView.class}),
    INVOICE_LIST("Invoice list", VaadinIcon.COPY_O, InvoicingView.class, new Class[]{InvoicingView.class, InvoicingDetailView.class}),
    DS_LIST("DS message list", VaadinIcon.ENVELOPE_O, DsMessageView.class, new Class[]{DsMessageView.class, DsMessageDetailView.class}),
    EMPLOYEE_LIST("Employee list", VaadinIcon.GROUP, EmployeeView.class, new Class[]{EmployeeView.class, EmployeeDetailView.class}),

    TASK_LIST("Task list", VaadinIcon.CALENDAR_CLOCK, TaskView.class, new Class[]{TaskView.class}),
    DOCUMENT_LIST("Document list", VaadinIcon.FILE_SEARCH, DocumentView.class, new Class[]{DocumentView.class}),
    TASK_TEMPLATE_LIST("Task template list", VaadinIcon.FILE_TEXT, TaskTemplateView.class, new Class[]{TaskTemplateView.class}),
    TASK_SCHEDULE_LIST("Task schedule list", VaadinIcon.CLOCK, TaskScheduleView.class, new Class[]{TaskScheduleView.class}),

    ROLE_LIST("Role list", VaadinIcon.USER_CHECK, RoleView.class, new Class[]{RoleView.class, RoleDetailView.class}),
    CUSTOM_PERMISSION("Custom permission", VaadinIcon.UNLOCK, CustomPermissionView.class, new Class[]{CustomPermissionView.class}),
    PERMISSION_MANAGEMENT("Permission management", VaadinIcon.UNLOCK, PermissionManagementView.class, new Class[]{PermissionManagementView.class}),
    USER_LIST("User list", VaadinIcon.USERS, UserView.class, new Class[]{UserView.class, UserDetailView.class}),

    APROVEMENT_APROVER("Aprovement", VaadinIcon.GAVEL, ApprovementApproverView.class, new Class[]{ApprovementApproverView.class}),
    APROVEMENT_HOLIDAY(ApprovementType.HOLIDAY.name(), ApprovementType.HOLIDAY.getIcon(), ApprovementHolidayView.class, new Class[]{ApprovementHolidayView.class}),
    APROVEMENT_HOME_OFFICE(ApprovementType.HOME_OFFICE.name(), ApprovementType.HOME_OFFICE.getIcon(), ApprovementHomeOfficeView.class, new Class[]{ApprovementHomeOfficeView.class}),
    APROVEMENT_BUSINESS_TRIP(ApprovementType.BUSSINES_TRIP.name(), ApprovementType.BUSSINES_TRIP.getIcon(), ApprovementBusinessTripView.class, new Class[]{ApprovementBusinessTripView.class}),
    APROVEMENT_UNPAID_LEAVE(ApprovementType.UNPAID_LEAVE.name(), ApprovementType.UNPAID_LEAVE.getIcon(), ApprovementUnpaidLeaveView.class, new Class[]{ApprovementUnpaidLeaveView.class}),
    APROVEMENT_PAID_LEAVE(ApprovementType.PAID_LEAVE.name(), ApprovementType.PAID_LEAVE.getIcon(), ApprovementPaidLeaveView.class, new Class[]{ApprovementPaidLeaveView.class}),
    APROVEMENT_ILL(ApprovementType.ILL.name(), ApprovementType.ILL.getIcon(), ApprovementIllView.class, new Class[]{ApprovementIllView.class}),
    VIRTUAL_SERVER("Virtual server", VaadinIcon.SERVER, VirtualServerView.class, new Class[]{VirtualServerView.class}),
    USER_SETTINGS("User settings", VaadinIcon.USER_CARD, UserSettingsView.class, new Class[]{UserSettingsView.class}),

    WORK_REPORT("Work report", VaadinIcon.CLIPBOARD_PULSE, WorkReportView.class, new Class[]{WorkReportView.class}),

    ENUMERATIONS("Enumerations", VaadinIcon.GRID_SMALL, AdministrationView.class,
            new Class[]{
                    ContractTypeDetailView.class,
                    ContactPersonTypeDetailView.class,
                    DocumentTypeDetailView.class,
                    SupplierTypeDetailView.class,
                    AssetPositionDetailView.class,
                    DsSettingDetailView.class,
                    AreaDetailView.class,
                    TechnologyDetailView.class,
                    DphDetailView.class,
                    AdministrationView.class

            }),
    LABEL_LIST("Label list", VaadinIcon.COPY_O, LabelView.class, new Class[]{LabelView.class, LabelDetailView.class}),

    APP_LOG_TAB("App log", VaadinIcon.ALIGN_CENTER, AppLogView.class, new Class[]{AppLogTab.class, AppLogView.class}, AppLogTab.TAB_INDEX),
    SCHEDULER_LOG_TAB("Scheduler log", VaadinIcon.TIME_FORWARD, AppLogView.class, new Class[]{SchedulerLogTab.class, AppLogView.class}, SchedulerLogTab.TAB_INDEX),

    ATTENDANCE_LIST("Attendance", VaadinIcon.USER_CLOCK, AttendanceView.class, new Class[]{AttendanceView.class}),
    TRANSLATION_LIST("Translations", VaadinIcon.COMMENT_ELLIPSIS_O, TranslationView.class,
            new Class[]{TranslationView.class});

    private final String text;
    private final VaadinIcon icon;
    private final Class<? extends Component> view;
    private final Class<? extends Component>[] viewArray;

    private final Integer tabIndex;

    SubMenuItem(String text, VaadinIcon icon, Class<? extends Component> view,
                Class<? extends Component>[] viewArray, Integer tabIndex) {
        this.text = text;
        this.view = view;
        this.viewArray = viewArray;
        this.tabIndex = tabIndex;
        this.icon = icon;
    }


    SubMenuItem(String text, VaadinIcon icon, Class<? extends Component> view, Class<? extends Component>[] viewArray) {
        this.text = text;
        this.view = view;
        this.viewArray = viewArray;
        this.tabIndex = null;
        this.icon = icon;
    }

    public String getText() {
        return text;
    }

    public Integer getTabIndex() {
        return tabIndex;
    }

    public Class<? extends Component> getView() {
        return view;
    }

    public Class<? extends Component>[] getViewArray() {
        return viewArray;
    }

    public VaadinIcon getIcon() {
        return icon;
    }

    public static SubMenuItem findSubmenuMenuItemByClass(Class<AppView> clazz) {
        SubMenuItem subMenuItem = null;
        for (int i = 0; i < values().length; i++) {
            if (values()[i].getView() == clazz) {
                subMenuItem = values()[i];
            }
        }
        return subMenuItem;
    }

    public static VaadinIcon getIconByClass(Class<AppView> clazz) {
        VaadinIcon vaadinIcon = null;
        for (int i = 0; i < values().length; i++) {
            if (Arrays.stream(values()[i].getViewArray()).toList().contains(clazz)) {
                vaadinIcon = values()[i].icon;
            }
        }
        if (vaadinIcon == null) {
            for (int i = 0; i < MenuItem.values().length; i++) {
                if (MenuItem.values()[i].getView() == clazz) {
                    vaadinIcon = MenuItem.values()[i].getIcon();
                }
            }
        }
        return vaadinIcon;
    }
}
