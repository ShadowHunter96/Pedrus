package cz.bbn.cerberus.commons;

import cz.bbn.cerberus.translation.Transl;

import java.util.LinkedHashSet;
import java.util.Set;

public enum DomainEnum {

    EMPTY_DOMAIN("", "", false),
    DASHBOARD_DOMAIN_NAME("Dashboard", "Dashboard", true),
    MANAGEMENT_DASHBOARD_DOMAIN_NAME("Management dashboard", "Management Dashboard", true),
    SALES_DASHBOARD_DOMAIN_NAME("Sales dashboard", "Sales Dashboard", true),
    EMAIL_DOMAIN_NAME("Email", "Email", true),
    SUBJECT_DOMAIN_NAME("Subject", "Subject", true),
    SUPPLIER_DOMAIN_NAME("Supplier", "Supplier", true),
    OPPORTUNITY_DOMAIN_NAME("Opportunity", "Opportunity", true),
    OFFER_DOMAIN_NAME("Offer", "Offer", true),
    CONTRACT_DOMAIN_NAME("Contract", "Contract", true),
    EMPLOYEE_CONTRACT_DOMAIN_NAME("Employee contract", "Employee contract", true),
    PROJECT_DOMAIN_NAME("Project", "Project", true),
    CONTACT_PERSON_DOMAIN_NAME("Contact person", "Contact person", true),
    ASSET_DOMAIN_NAME("Asset", "Asset", true),
    AREA_DOMAIN_NAME("Area", "Area", false),
    ASSET_POSITION_DOMAIN_NAME("Asset position", "Asset position", false),
    CONTACT_PERSON_TYPE_DOMAIN_NAME("Contact person type", "Contact person type", false),
    WORK_REPORT_DOMAIN_NAME("Work report", "Work report", true),
    CONTRACT_TYPE_DOMAIN_NAME("Contract type", "Contract type", false),
    DOCUMENT_DOMAIN_NAME("Document", "Document", true),
    DOCUMENT_TYPE_DOMAIN_NAME("Document type", "Document type", false),
    DPH_DOMAIN_NAME("Dph", "Dph", false),
    DS_MESSAGE_DOMAIN_NAME("DS message", "DS message", true),
    DS_SETTING_DOMAIN_NAME("DS setting", "DS setting", true),
    EMPLOYEE_DOMAIN_NAME("Employee", "Employee", true),
    ACTIVITY_DOMAIN_NAME("Activity", "Activity", true),
    ENUMERATION_DOMAIN_NAME("Enumeration", "Enumeration", false),
    NOTE_DOMAIN_NAME("Note", "Note", true),
    TASK_DOMAIN_NAME("Task", "Event", true),
    TASK_FOLLOWING_DOMAIN_NAME("Task following", "Task following", false),
    INVOICE_DOMAIN_NAME("Invoice", "Invoice", true),
    LABEL_DOMAIN_NAME("Label", "Label", false),
    PERMISSION_GROUP_DOMAIN_NAME("Permission group", "Permission group", false),
    PHASE_DOMAIN_NAME("Phase", "Phase", false),
    ROLE_DOMAIN_NAME("Role", "Role", true),
    SUPPLIER_TYPE_DOMAIN_NAME("Supplier type", "Supplier type", true),
    TECHNOLOGY_DOMAIN_NAME("Technology", "Technology", false),
    USER_DOMAIN_NAME("User", "User", false),
    ADMINISTRATION_DOMAIN_NAME("Administration", "Administration", true),
    APPROVEMENT("Approvement", "Approvement", true),
    INFRASTRUCTURE("Infrastructure", "Infrastructure", true),
    ATTENDANCE("Attendance", "Attendance", true),
    ;

    private final String value;
    private final String nameForTransl;
    private final boolean permissionDomain;

    DomainEnum(String value, String nameForTransl, boolean permissionDomain) {
        this.value = value;
        this.nameForTransl = nameForTransl;
        this.permissionDomain = permissionDomain;
    }

    public String getValue() {
        return value;
    }

    public String getNameForTransl() {
        return nameForTransl;
    }

    public String getTranslatedName() {
        return Transl.get(nameForTransl);
    }

    public static Set<DomainEnum> getPermissionDomainSet() {
        Set<DomainEnum> domainSet = new LinkedHashSet<>();
        for (DomainEnum domainEnum : DomainEnum.values()) {
            if (domainEnum.permissionDomain) {
                domainSet.add(domainEnum);
            }
        }
        return domainSet;
    }

    public static DomainEnum getDomainByValue(String value) {
        for (DomainEnum domainEnum : DomainEnum.values()) {
            if (domainEnum.getValue().equals(value)) {
                return domainEnum;
            }
        }
        return EMPTY_DOMAIN;
    }

}
