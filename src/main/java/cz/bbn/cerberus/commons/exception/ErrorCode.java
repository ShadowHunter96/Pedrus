package cz.bbn.cerberus.commons.exception;

import lombok.extern.slf4j.Slf4j;

import java.text.MessageFormat;
import java.util.Arrays;

@Slf4j
public enum ErrorCode implements ErrorInterface {

    INVALID_PROPERTY_VALUE("Invalid property value, expected {0} but provided {1}. Property: {2}."),
    ACTION_NOT_SUPPORTED("Action {0} not supported for {1} grid"),
    ROLE_NOT_EXISTS("Role {0} not exists"),
    ROLE_ALREADY_EXISTS("Role {0} already exists"),
    PROJECT_NOT_EXISTS("Project {0} not exists"),
    ID_ALREADY_EXISTS("Id {0} already exists"),
    CONTACT_PERSON_NOT_EXISTS("Contact person {0} not exists"),
    CONTACT_PERSON_TYPE_NOT_EXISTS("Contact person type {0} not exists"),
    CONTRACT_TYPE_NOT_EXISTS("Contract type {0} not exists"),
    DOCUMENT_TYPE_NOT_EXISTS("Document type {0} not exists"),
    DOCUMENT_NOT_EXISTS("Document {0} not exists"),
    SUBJECT_NOT_EXISTS("Subject {0} does not exist"),
    SUBJECT_ALREADY_EXISTS("Subject with {0} already exists"),
    SUBJECT_CHOOSE_ONE_BOOLEAN("You have to choose one of this checkbox: customer, supplier, own company"),
    NOTE_NOT_EXISTS("Note not exists"),
    NOTE_ALREADY_EXISTS("Note already exists"),
    DOCUMENT_NAME_EXISTS("Document with name {0} already exists. Add versioning please"),
    DOCUMENT_NAME_SIZE_ALREADY_EXISTS("Document with same name {0} and same size already exists. You can linked it"),
    DOCUMENT_FILE_IS_NOT_FILLED("Document file is empty or is not filled"),
    DOCUMENT_FILE_NOT_EXISTS("Document file not exists"),
    CONTRACT_NOT_EXISTS("Contract {0} does not exist"),
    CONTRACT_ALREADY_EXISTS("Contract with {0} already exists"),
    ITSELF_CONNECT_ERROR("The contract cannot be connected with itself"),
    CONNECTED_CONTRACT_BAD_SUBJECT_ERROR("The connected contract does not have the same subject"),
    UNKNOWN_LOGIN_ERROR("Login error: global error"),
    BAD_LDAP_ROLE("User has no role in ldap"),
    LOGIN_ERROR("Login error: {0}"),
    INVOICE_DOES_NOT_EXIST("Invoice {0} does not exists"),
    INVOICE_ALREADY_EXISTS("Invoice {0} already exists"),
    SUPPLIER_TYPE_NOT_EXISTS("Supplier type {0} does not exist"),
    SUPPLIER_NOT_EXISTS("Supplier {0} does not exist"),
    SUPPLIER_ALREADY_EXISTS("Supplier {0} already exists"),
    ASSET_NOT_EXITS("Asset {0} does not exists"),
    ARES_ANSWER_ERROR("Error getting data from Ares"),
    ASSET_POSITION_NOT_EXITS("Asset position {0} does not exists"),
    TASK_NOT_EXISTS("Task {0} not exists"),
    TASK_FOLLOWING_NOT_EXISTS("Following user not exists"),
    PERMISSION_GROUP_DOES_NOT_EXIST("Permission group {0} does not exist"),
    PERMISSION_GROUP_ALREADY_EXISTS("Permission group {0} already exists"),
    OPPORTUNITY_NOT_EXITS("Opportunity {0} does not exists"),
    USER_NOT_EXISTS("User {0} not exists"),
    DS_SETTINGS_NOT_EXISTS("Ds setting {0} not exists"),
    DS_MESSAGE_NOT_EXISTS("Ds message {0} not exists"),
    AREA_NOT_EXITS("Area {0} does not exists"),
    LABEL_NOT_EXITS("Label {0} does not exists"),
    LABEL_SUBJECT_NOT_EXITS("Subject label does not exists"),
    TECHNOLOGY_NOT_EXITS("Technology {0} does not exists"),
    TECHNOLOGY_OPPORTUNITY_IS_LINKED("The opportunity is already linked with technology {0}"),
    AREA_OPPORTUNITY_IS_LINKED("The opportunity is already linked with area {0}"),
    TECHNOLOGY_OPPORTUNITY_NOT_EXITS("Technology in opportunity not exists"),
    DPH_NOT_EXITS("DPH {0} does not exists"),
    OFFER_NOT_EXITS("Label {0} does not exists"),
    ENUMERATION_NOT_EXISTS("Enumeration {0} not exists"),
    ASSET_CONNECTION_NOT_EXISTS("Asset connection id {0} does not exists"),
    EMPLOYEE_DOES_NOT_EXISTS("Employee id {0} does not exists"),
    EMPLOYEE_ALREADY_EXISTS("Employee id {0} already exists"),
    DEFAULT_VALUE_ASSIGNED("The default value is already assigned to entity {0} "),
    PHASE_NOT_EXITS("Phase {0} does not exists"),
    EMPLOYEE_NOT_FOUND("Employee entity for this user not found"),
    AREA_TECHNOLOGY_SIGN_EXISTS("This sign already exists"),
    VIEW_PERMISSION_MISSING("You do not have permission to view this object"),
    EMAIL_NOT_EXISTS("Email {0} does not exists"),
    SAVE_EMAIL_ERROR("Email occurred while saving email"),
    APPROVEMENT_NOT_EXITS("Approvement does not exists"),
    UNSET_BACK_OFFICE("No role is set as back office"),
    UNSET_INFRASTRUCTURE("No role is set as infrastructure"),
    VIRTUAL_SERVER_NOT_EXISTS("Virtual server {0} does not exists"),
    VIRTUAL_SERVER_ALREADY_EXISTS("Virtual server id {0} already exists"),
    EMPLOYEE_CONTRACT_NOT_EXISTS("Employee contract id {0} does not exists"),
    ATTENDANCE_DOCUMENT_NOT_EXITS("Attendance document does not exists"),
    OWN_COMPANY_ALREADY_EXISTS("Only one subject can be own company. Subject {0} is already own company"),
    TRANSLATION_DOES_NOT_EXISTS("Translation id {0} does not exists"),
    EMPLOYEE_ALREADY_ASSIGNED("Picked employee is already assigned to another user"),
    TASK_TEMPLATE_NOT_EXISTS("Task template {0} does not exists"),
    TASK_SCHEDULE_NOT_EXISTS("Task schedule {0} does not exists"),
    TASK_TYPE_NOT_EXISTS("Task type {0} does not exists");

    private final String code;

    private String message;

    ErrorCode(String code) {
        this.code = code;
    }

    @Override
    public String getError() {
        return code;
    }

    public String getMessage(Object... params) {
        try {
            return MessageFormat.format(message, params);
        } catch (IllegalArgumentException e) {
            log.error("Wrong formatted error code message: " + message, e);
            return message + ", params: " + Arrays.toString(params);
        }
    }
}
