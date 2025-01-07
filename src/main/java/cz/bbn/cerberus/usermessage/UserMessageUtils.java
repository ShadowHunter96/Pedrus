package cz.bbn.cerberus.usermessage;

import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.contract.ui.ContractSalesDetailView;
import cz.bbn.cerberus.project.ui.ProjectDetailView;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class UserMessageUtils {

    private static final String LESS_THEN = "Less than ";
    public static final String BACKGROUND_COLOR = "background-color";
    public static final String HSL = ", 63%, 83%)";

    private UserMessageUtils() {
    }

    public static String getDaysRemaining(LocalDate dueDate, AppEnv appEnv) {
        LocalDate actualDate = LocalDate.now();
        long days = ChronoUnit.DAYS.between(actualDate, dueDate);
        String daysRemaining = "";
        if (days <= appEnv.getUserMessageDaysLevel1()) {
            daysRemaining = LESS_THEN.concat(String.valueOf(appEnv.getUserMessageDaysLevel1()));
        }

        if (days <= appEnv.getUserMessageDaysLevel2()) {
            daysRemaining = LESS_THEN.concat(String.valueOf(appEnv.getUserMessageDaysLevel2()));
        }

        if (days <= appEnv.getUserMessageDaysLevel3()) {
            daysRemaining = LESS_THEN.concat(String.valueOf(appEnv.getUserMessageDaysLevel3()));
        }

        return daysRemaining;
    }

    public static String getBodyStyle(LocalDate dueDate, AppEnv appEnv) {
        LocalDate actualDate = LocalDate.now();
        long days = ChronoUnit.DAYS.between(actualDate, dueDate);

        String bodyStyle = null;

        if (days <= appEnv.getUserMessageDaysLevel1()) {
            bodyStyle = appEnv.getUserMessageColorLevel1();
        }

        if (days <= appEnv.getUserMessageDaysLevel2()) {
            bodyStyle = appEnv.getUserMessageColorLevel2();
        }
        if (days <= appEnv.getUserMessageDaysLevel3()) {
            bodyStyle = appEnv.getUserMessageColorLevel3();
        }

        return bodyStyle;
    }

    public static String getHeaderStyle(boolean isPriority, AppEnv appEnv) {
        if (isPriority) {
            return appEnv.getUserMessageColorPriority();
        } else {
            return appEnv.getUserMessageColorNormal();
        }
    }

    public static String getUserMessageLink(UserMessageObjectType type, String objectId) {
        switch (type) {

            case ALL:
                return null;

            case SUBJECT:
                return SubjectDetailView.ROUTE.concat("/").concat(objectId);

            case PROJECT:
                return ProjectDetailView.ROUTE.concat("/").concat(objectId);

            case CONTRACT:
                return ContractSalesDetailView.ROUTE.concat("/").concat(objectId);

            default:
                return "";
        }
    }
}
