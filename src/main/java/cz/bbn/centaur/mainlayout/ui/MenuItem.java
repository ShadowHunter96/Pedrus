package cz.bbn.cerberus.mainlayout.ui;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.dashboard.ui.DashboardView;
import cz.bbn.cerberus.marekdemo.ui.MarekView;


public enum MenuItem {

    DASHBOARD("Dashboard", VaadinIcon.CALENDAR_USER, DashboardView.class, new Class[]{DashboardView.class}, false),
    MAREK("Marek", VaadinIcon.ABACUS, MarekView.class, new Class[]{MarekView.class}, true),
    MANAGEMENT("Management", VaadinIcon.SITEMAP,
            new SubMenuItem[]{
                    SubMenuItem.MANAGEMENT_DASHBOARD,
                    SubMenuItem.MANAGEMENT_INTRANET_DASHBOARD,
                    SubMenuItem.MANAGEMENT_OWNER,
            }, false),
    SALES("Sales", VaadinIcon.EURO,
            new SubMenuItem[]{
                    SubMenuItem.SUBJECT_LIST,
                    SubMenuItem.OPPORTUNITY_LIST,
                    SubMenuItem.OFFER_LIST,
                    SubMenuItem.CONTRACT_LIST,
                    SubMenuItem.INVOICE_LIST,
                    SubMenuItem.PROJECT_LIST,
                    SubMenuItem.CONTACT_PERSON_LIST,
                    SubMenuItem.TASK_LIST,
                    SubMenuItem.NOTE_LIST,
                    SubMenuItem.EMAIL_LIST,
            }, false),

    BACKOFFICE("Backoffice", VaadinIcon.OFFICE,
            new SubMenuItem[]{
                    SubMenuItem.ATTENDANCE_LIST,
                    SubMenuItem.BO_CONTRACT_LIST,
                    SubMenuItem.EMP_CONTRACT_LIST,
                    SubMenuItem.ASSET_LIST,
                    SubMenuItem.DS_LIST,
                    SubMenuItem.EMPLOYEE_LIST
            }, false),

    INTRANET("User", VaadinIcon.CLIPBOARD_HEART,
            new SubMenuItem[]{
                    SubMenuItem.WORK_REPORT,
                    SubMenuItem.APROVEMENT_APROVER,
                    SubMenuItem.APROVEMENT_HOLIDAY,
                    SubMenuItem.APROVEMENT_HOME_OFFICE,
                    SubMenuItem.APROVEMENT_BUSINESS_TRIP,
                    SubMenuItem.APROVEMENT_UNPAID_LEAVE,
                    SubMenuItem.APROVEMENT_PAID_LEAVE,
                    SubMenuItem.APROVEMENT_ILL,
                    SubMenuItem.VIRTUAL_SERVER,
                    SubMenuItem.USER_SETTINGS,
            }, false),

    OTHER("Other", VaadinIcon.ALIGN_JUSTIFY,
            new SubMenuItem[]{
                    SubMenuItem.DOCUMENT_LIST,
                    SubMenuItem.TASK_TEMPLATE_LIST,
                    SubMenuItem.TASK_SCHEDULE_LIST,
            }, true),

    USER("IAM", VaadinIcon.CLIPBOARD_USER,
            new SubMenuItem[]{
                    SubMenuItem.ROLE_LIST,
                    SubMenuItem.PERMISSION_MANAGEMENT,
                    SubMenuItem.USER_LIST
            }, false),


    ADMINISTRATION("Administration", VaadinIcon.TOOLS,
            new SubMenuItem[]{
                    SubMenuItem.ENUMERATIONS,
                    SubMenuItem.LABEL_LIST,
                    SubMenuItem.TRANSLATION_LIST,
            }, false),

    APP_LOG("Logs", VaadinIcon.NOTEBOOK,
            new SubMenuItem[]{
                    SubMenuItem.APP_LOG_TAB,
                    SubMenuItem.SCHEDULER_LOG_TAB
            }, false);


    private final String text;
    private final VaadinIcon icon;
    private final Class<? extends Component> view;
    private final Class<? extends Component>[] viewArray;
    private final boolean addSeparator;

    private final SubMenuItem[] subMenuItems;

    MenuItem(String text, VaadinIcon icon, Class<? extends Component> view,
             Class<? extends Component>[] viewArray, boolean addSeparator) {
        this.text = text;
        this.icon = icon;
        this.view = view;
        this.addSeparator = addSeparator;
        this.viewArray = viewArray;
        subMenuItems = new SubMenuItem[0];
    }


    MenuItem(String text, VaadinIcon icon,
             SubMenuItem[] subMenuItems, boolean addSeparator) {
        this.text = text;
        this.icon = icon;
        this.addSeparator = addSeparator;
        this.subMenuItems = subMenuItems;
        this.view = null;
        this.viewArray = null;
    }


    public String getText() {
        return text;
    }

    public VaadinIcon getIcon() {
        return icon;
    }

    public Class<? extends Component> getView() {
        return view;
    }

    public Class<? extends Component>[] getViewList() {
        return viewArray;
    }

    public boolean getAddSeparator() {
        return addSeparator;
    }

    public SubMenuItem[] getSubMenuItems() {
        return subMenuItems;
    }

}
