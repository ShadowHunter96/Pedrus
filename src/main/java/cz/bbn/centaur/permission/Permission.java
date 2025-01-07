package cz.bbn.cerberus.permission;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.GrantedAuthority;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Slf4j
public enum Permission implements GrantedAuthority {

    NON_EXISTENT_PERMISSION(DomainEnum.EMPTY_DOMAIN, false, false, "", ""),

    DASHBOARD(DomainEnum.DASHBOARD_DOMAIN_NAME, false, true,
            "Dashboard view", "Can view dashboard, changelog and can reload cash"),

    MANAGEMENT_DASHBOARD(DomainEnum.MANAGEMENT_DASHBOARD_DOMAIN_NAME, false, true,
            "Management Dashboard view", "Can view management dashboard"),

    MANAGEMENT_OWNER_VIEW(DomainEnum.MANAGEMENT_DASHBOARD_DOMAIN_NAME, false, true,
            "Owner management", "Can view owner management"),

    SALES_DASHBOARD(DomainEnum.SALES_DASHBOARD_DOMAIN_NAME, false, true,
            "Sales Dashboard view", "Can view sales dashboard"),

    ROLE_VIEW(DomainEnum.ROLE_DOMAIN_NAME, false, true,
            "Role view", "Can view roles and role details"),
    ROLE_EDIT(DomainEnum.ROLE_DOMAIN_NAME, false, false,
            "Role edit", "Can create and edit role"),
    ROLE_DELETE(DomainEnum.ROLE_DOMAIN_NAME, false, false,
            "Role delete", "Can delete role"),

    CUSTOM_PERMISSION_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Custom permission view", "Can view and edit custom permissions"),

    PROJECT_VIEW(DomainEnum.PROJECT_DOMAIN_NAME, true, true,
            "Project view", "Can view project and project detail"),
    PROJECT_LIST_VIEW(DomainEnum.PROJECT_DOMAIN_NAME, false, false,
            "Project list view", "Can view project list"),
    PROJECT_EDIT(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project edit", "Can create and edit project"),
    PROJECT_DELETE(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project delete", "Can delete project"),
    PROJECT_SHOW_DELETED(DomainEnum.PROJECT_DOMAIN_NAME, false, false,
            "Show deleted projects", "Can view deleted projects"),

    PROJECT_EMAIL_VIEW(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project email view", "Can view project emails"),
    PROJECT_EMAIL_EDIT(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project email edit", "Can edit project emails"),
    PROJECT_EMAIL_DELETE(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project email delete", "Can delete project emails"),

    PROJECT_CHANGE_CUS_CONT(DomainEnum.PROJECT_DOMAIN_NAME, false, false,
            "Project change customer and contract", "Can change customer and contract on project"),

    PROJECT_LINK_CONTACT_PERSON(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project link contact person", "Can link contact person to project"),

    CONTACT_PERSON_VIEW(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, true, true,
            "Contact person view", "Can view contact person and contact person detail"),
    CONTACT_PERSON_EDIT(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, true, false,
            "Contact person edit", "Can create and edit contact person"),
    CONTACT_PERSON_DELETE(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, true, false,
            "Contact person delete", "Can delete contact person"),
    CONTACT_PERSON_SHOW_DELETED(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, false, false,
            "Show deleted contact persons", "Can view deleted contact persons"),

    ADMINISTRATION_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Administration view", "Can view administration"),

    CONTACT_PERSON_TYPE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Contact person type view", "Can view contact person types and contact person types detail"),
    CONTACT_PERSON_TYPE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Contact person type edit", "Can create and edit contact persons"),
    CONTACT_PERSON_TYPE_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Show deleted contact person types", "Can view deleted contact person types"),

    DOCUMENT_TYPE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Document type view", "Can view document types and document types detail"),
    DOCUMENT_TYPE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Document type edit", "Can edit and create document types"),
    DOCUMENT_TYPE_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Document type delete", "Can view deleted document types"),

    SUBJECT_VIEW(DomainEnum.SUBJECT_DOMAIN_NAME, true, true,
            "Subject view", "Can view subject and subject detail"),
    SUBJECT_EDIT(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject edit", "Can edit and create subject"),
    SUBJECT_DELETE(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject delete", "Can delete subject"),
    SUBJECT_PREVIEW(DomainEnum.SUBJECT_DOMAIN_NAME, false, true,
            "Subject preview", "This permission will be deleted"), // smazat

    SUBJECT_EMAIL_VIEW(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject email view", "Can view subject emails"),
    SUBJECT_EMAIL_EDIT(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject email edit", "Can edit subject emails"),
    SUBJECT_EMAIL_DELETE(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject email delete", "Can delete subject emails"),

    SUBJECT_LINK_CONTACT_PERSON(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject link contact person", "Can link contact person to subject"),

    SUBJECT_CHANGE_SUBJECT_TYPE(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Change subject type", "Change subject type"),

    DOCUMENT_VIEW(DomainEnum.DOCUMENT_DOMAIN_NAME, false, true,
            "Document view", "Can view documents and documents detail"),
    DOCUMENT_EDIT(DomainEnum.DOCUMENT_DOMAIN_NAME, false, false,
            "Document edit", "Can edit and create documents"),
    DOCUMENT_DELETE(DomainEnum.DOCUMENT_DOMAIN_NAME, false, false,
            "Document delete", "Can delete document"),
    DOCUMENT_DOWNLOAD(DomainEnum.DOCUMENT_DOMAIN_NAME, false, false,
            "Document download", "Can download documents"),

    CONTACT_PERSON_NOTE_VIEW(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, true, true,
            "Contact person note view", "Can view contact person notes and contact person notes detail"),
    CONTACT_PERSON_NOTE_EDIT(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, true, false,
            "Contact person note edit", "Can edit and create contact person notes"),
    CONTACT_PERSON_NOTE_DELETE(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, true, false,
            "Contact person note delete", "Can delete contact person note"),

    PROJECT_NOTE_VIEW(DomainEnum.PROJECT_DOMAIN_NAME, true, true,
            "Project note view", "Can view project notes and project notes detail"),
    PROJECT_NOTE_EDIT(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project note edit", "Can edit and create project notes"),
    PROJECT_NOTE_DELETE(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project note delete", "Can delete project note"),

    SUBJECT_NOTE_VIEW(DomainEnum.SUBJECT_DOMAIN_NAME, true, true,
            "Subject note view", "Can view subject notes and subject notes detail"),
    SUBJECT_NOTE_EDIT(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject note edit", "Can edit and create subject notes"),
    SUBJECT_NOTE_DELETE(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject note delete", "Can delete subject note"),


    PROJECT_DOCUMENT_VIEW(DomainEnum.PROJECT_DOMAIN_NAME, true, true,
            "Project document view", "Can view project documents and project documents detail"),
    PROJECT_DOCUMENT_UPLOAD(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project document upload", "Can upload project documents"),
    PROJECT_DOCUMENT_DELETE(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project document delete", "Can delete project document"),
    PROJECT_DOCUMENT_DOWNLOAD(DomainEnum.PROJECT_DOMAIN_NAME, true, false,
            "Project document download", "Can download project documents"),

    CONTACT_PERSON_DOCUMENT_VIEW(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, false, true,
            "Contact person document view", "Can view contact person documents and contact person documents detail"),
    CONTACT_PERSON_DOCUMENT_UPLOAD(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, false, false,
            "Contact person document upload", "Can upload contact person documents"),
    CONTACT_PERSON_DOCUMENT_DELETE(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, false, false,
            "Contact person document delete", "Can delete contact person documents"),
    CONTACT_PERSON_DOCUMENT_DOWNLOAD(DomainEnum.CONTACT_PERSON_DOMAIN_NAME, false, false,
            "Contact person document download", "Can download contact persons documents"),

    SUBJECT_DOCUMENT_VIEW(DomainEnum.SUBJECT_DOMAIN_NAME, true, true,
            "Subject document view", "Can view subject documents and subject documents detail"),
    SUBJECT_DOCUMENT_UPLOAD(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject document upload", "Can upload subject documents"),
    SUBJECT_DOCUMENT_DELETE(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject document delete", "Can delete subject documents"),
    SUBJECT_DOCUMENT_DOWNLOAD(DomainEnum.SUBJECT_DOMAIN_NAME, true, false,
            "Subject document download", "Can download subject documents"),

    ICO_CHANGE(DomainEnum.SUBJECT_DOMAIN_NAME, false, false,
            "ICO change", "Can change ICO on subjects"),

    APP_LOG_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "App log view", "Can view app logs"),

    CONTRACT_VIEW(DomainEnum.CONTRACT_DOMAIN_NAME, true, true,
            "Contact view", "Can view contract and contract detail"),
    CONTRACT_EDIT(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract edit", "Can edit and create contract"),
    CONTRACT_DELETE(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract delete", "Can delete contract"),
    CONTRACT_SHOW_DELETED(DomainEnum.CONTRACT_DOMAIN_NAME, false, false,
            "Contract show deleted", "Can view deleted contracts"),

    SALES_CONTRACT_VIEW(DomainEnum.CONTRACT_DOMAIN_NAME, false, false,
            "Sales contract view", "Can view sales contract and sales contract detail"),
    SALES_CONTRACT_LIST_VIEW(DomainEnum.CONTRACT_DOMAIN_NAME, false, false,
            "Sales contract list view", "Can view sales contract list"),

    BO_CONTRACT_VIEW(DomainEnum.CONTRACT_DOMAIN_NAME, false, false,
            "BO contract view", "Can view back office contract and back office contract detail"),
    BO_CONTRACT_LIST_VIEW(DomainEnum.CONTRACT_DOMAIN_NAME, false, false,
            "BO contract list view", "Can view back office contract list"),

    CONTRACT_UPLOAD_TO_POHODA(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Upload contract to Pohoda", "Can upload contract to Pohoda"),

    CONTRACT_NOTE_VIEW(DomainEnum.CONTRACT_DOMAIN_NAME, true, true,
            "Contract note view", "Can view contract notes and contract notes detail"),
    CONTRACT_NOTE_EDIT(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract note edit", "Can edit and create contract notes"),
    CONTRACT_NOTE_DELETE(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract note delete", "Can delete contract note"),

    CONTRACT_DOCUMENT_VIEW(DomainEnum.CONTRACT_DOMAIN_NAME, true, true,
            "Contract document view", "Can view contract documents and contract documents detail"),
    CONTRACT_DOCUMENT_UPLOAD(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract document upload", "Can upload contract documents"),
    CONTRACT_DOCUMENT_DELETE(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract document delete", "Can delete contract document"),
    CONTRACT_DOCUMENT_DOWNLOAD(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract document download", "Can download contract documents"),

    CONTRACT_AREA_TECHNOLOGY_SIGN_EDIT(DomainEnum.CONTRACT_DOMAIN_NAME, false, false,
            "Contract area technology sign", "Can add sign for area and technology to contract"),

    CONTRACT_EMAIL_VIEW(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract email view", "Can view contract emails"),
    CONTRACT_EMAIL_EDIT(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract email edit", "Can edit contract emails"),
    CONTRACT_EMAIL_DELETE(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract email delete", "Can delete contract emails"),

    CONTRACT_LINK_CONTACT_PERSON(DomainEnum.CONTRACT_DOMAIN_NAME, true, false,
            "Contract link contact person", "Can link contact person to contract"),

    SUPPLIER_VIEW(DomainEnum.SUPPLIER_DOMAIN_NAME, false, true,
            "Supplier view", "Can view supplier and supplier detail"),
    SUPPLIER_EDIT(DomainEnum.SUPPLIER_DOMAIN_NAME, false, false,
            "Supplier edit", "Can edit and create suppliers"),

    SUPPLIER_TYPE_VIEW(DomainEnum.SUPPLIER_TYPE_DOMAIN_NAME, false, true,
            "Supplier type view", "Can view supplier type and supplier type detail"),
    SUPPLIER_TYPE_EDIT(DomainEnum.SUPPLIER_TYPE_DOMAIN_NAME, false, false,
            "Supplier type edit", "Can edit and create supplier type"),
    SUPPLIER_TYPE_DELETE(DomainEnum.SUPPLIER_TYPE_DOMAIN_NAME, false, false,
            "Supplier type delete", "Can delete supplier type"),

    ASSET_VIEW(DomainEnum.ASSET_DOMAIN_NAME, false, true,
            "Asset view", "Can view asset and asset detail"),
    ASSET_EDIT(DomainEnum.ASSET_DOMAIN_NAME, false, false,
            "Asset edit", "Can edit and create asset"),
    ASSET_DELETE(DomainEnum.ASSET_DOMAIN_NAME, false, false,
            "Asset delete", "Can delete asset"),
    ASSET_SHOW_DELETED(DomainEnum.ASSET_DOMAIN_NAME, false, false,
            "Asset show deleted", "Can view deleted assets"),

    ASSET_DOCUMENT_VIEW(DomainEnum.ASSET_DOMAIN_NAME, false, true,
            "Asset document view", "Can view asset documents and asset documents detail"),
    ASSET_DOCUMENT_UPLOAD(DomainEnum.ASSET_DOMAIN_NAME, false, false,
            "Asset document upload", "Can upload asset documents"),
    ASSET_DOCUMENT_DELETE(DomainEnum.ASSET_DOMAIN_NAME, false, false,
            "Asset document delete", "Can delete asset document"),
    ASSET_DOCUMENT_DOWNLOAD(DomainEnum.ASSET_DOMAIN_NAME, false, false,
            "Asset document download", "Can download asset documents"),

    SCHEDULER_LOG_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Scheduler log view", "Can view scheduler log"),

    TASK_VIEW(DomainEnum.TASK_DOMAIN_NAME, false, true,
            "Task view", "Can view tasks and tasks detail"),
    TASK_LIST_VIEW(DomainEnum.TASK_DOMAIN_NAME, false, false,
            "Task list view", "Can view all tasks"),
    TASK_EDIT(DomainEnum.TASK_DOMAIN_NAME, false, false,
            "Task edit", "Can edit and create tasks"),
    TASK_DELETE(DomainEnum.TASK_DOMAIN_NAME, false, false,
            "Task delete", "Can delete task"),

    TASK_TEMPLATE_VIEW(DomainEnum.TASK_DOMAIN_NAME, false, true,
            "Task template view", "Can view task templates and task templates detail"),
    TASK_TEMPLATE_EDIT(DomainEnum.TASK_DOMAIN_NAME, false, false,
            "Task template edit", "Can edit and create task templates"),
    TASK_TEMPLATE_DELETE(DomainEnum.TASK_DOMAIN_NAME, false, false,
            "Task template delete", "Can delete task template"),

    TASK_SCHEDULE_VIEW(DomainEnum.TASK_DOMAIN_NAME, false, true,
            "Task schedule view", "Can view task schedule and task schedule detail"),
    TASK_SCHEDULE_EDIT(DomainEnum.TASK_DOMAIN_NAME, false, false,
            "Task schedule edit", "Can edit and create task schedule"),
    TASK_SCHEDULE_DELETE(DomainEnum.TASK_DOMAIN_NAME, false, false,
            "Task schedule delete", "Can delete task schedule"),

    NOTE_VIEW(DomainEnum.NOTE_DOMAIN_NAME, false, true,
            "Note view", "Can view notes and notes detail"),

    OPPORTUNITY_VIEW(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, true,
            "Opportunity view", "Can view opportunity and opportunity detail"),
    OPPORTUNITY_EDIT(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity edit", "Can edit and create opportunity"),
    OPPORTUNITY_DELETE(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity delete", "Can delete opportunity"),
    OPPORTUNITY_SHOW_DELETED(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity show deleted", "Can view deleted opportunities"),

    OPPORTUNITY_LINK_CONTACT_PERSON(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity link contact person", "Can link contact person to opportunity"),

    OPPORTUNITY_NOTE_VIEW(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, true,
            "Opportunity note view", "Can view opportunity notes and opportunity notes detail"),
    OPPORTUNITY_NOTE_EDIT(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity notes edit", "Can edit and create opportunity notes"),
    OPPORTUNITY_NOTE_DELETE(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity note delete", "Can delete opportunity note"),

    OPPORTUNITY_DOCUMENT_VIEW(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, true,
            "Opportunity document view", "Can view opportunity documents and opportunity documents detail"),
    OPPORTUNITY_CREATE_CONTRACT(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity create contract", "Can create contract from opportunity"),

    OPPORTUNITY_TECHNOLOGY_SIGN_EDIT(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity area technology sign", "Can add sign for area and technology to opportunity"),

    OPPORTUNITY_COSTS_RETURN_FIELDS(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity fields costs and return", "Can view costs and return field"),

    OPPORTUNITY_EMAIL_VIEW(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity email view", "Can view opportunity emails"),
    OPPORTUNITY_EMAIL_EDIT(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity email edit", "Can edit opportunity emails"),
    OPPORTUNITY_EMAIL_DELETE(DomainEnum.OPPORTUNITY_DOMAIN_NAME, true, false,
            "Opportunity email delete", "Can delete opportunity emails"),


    PERMISSION_GROUP_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Permission group view", "Can view permission groups and permission groups detail"),
    PERMISSION_GROUP_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Permission group edit", "Can edit and create permission groups"),
    PERMISSION_GROUP_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Permission group delete", "Can delete permission group"),

    USER_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "User view", "Can view users and users detail"),
    USER_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "User show deleted", "Can view deleted users"),
    USER_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "User delete", "Can delete user"),
    USER_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "User edit", "Can edit and create users"),

    ASSET_POSITION_VIEW(DomainEnum.ASSET_DOMAIN_NAME, true, true,
            "Asset position view", "Can view asset positions and asset positions detail"),
    ASSET_POSITION_EDIT(DomainEnum.ASSET_DOMAIN_NAME, true, false,
            "Asset position edit", "Can edit and create asset positions"),
    ASSET_POSITION_DELETE(DomainEnum.ASSET_DOMAIN_NAME, true, false,
            "Asset position delete", "Can delete asset position"),
    ASSET_POSITION_SHOW_DELETED(DomainEnum.ASSET_DOMAIN_NAME, true, false,
            "Asset position show deleted", "Can view deleted asset positions"),

    INVOICE_VIEW(DomainEnum.INVOICE_DOMAIN_NAME, true, true,
            "Invoice view", "Can view invoice and invoice detail"),
    INVOICE_EDIT(DomainEnum.INVOICE_DOMAIN_NAME, true, false,
            "Invoice edit", "Can edit and create invoices"),
    INVOICE_TO_BE_INVOICED_ACTION(DomainEnum.INVOICE_DOMAIN_NAME, true, false,
            "To be invoiced action", "Can perform 'To be invoiced' action on invoice"),
    INVOICE_PAYED_ACTION(DomainEnum.INVOICE_DOMAIN_NAME, true, false,
            "Payed action", "Can perform 'Payed' action on invoice"),

    CAN_CHANGE_ASSET_LOCATION(DomainEnum.ASSET_DOMAIN_NAME, false, false,
            "Asset location change", "Can change asset location"),

    DS_SETTINGS_VIEW(DomainEnum.DS_SETTING_DOMAIN_NAME, true, true,
            "DS setting view", "Can view DS settings and DS settings detail"),
    DS_SETTINGS_EDIT(DomainEnum.DS_SETTING_DOMAIN_NAME, true, false,
            "DS setting edit", "Can edit and create DS settings"),
    DS_SETTINGS_DELETE_RECOVERY(DomainEnum.DS_SETTING_DOMAIN_NAME, false, false,
            "DS settings delete recovery", "Can recover deleted DS settings"),
    DS_SETTINGS_SHOW_DELETED(DomainEnum.DS_SETTING_DOMAIN_NAME, false, false,
            "DS settings show deleted", "Can view deleted DS settings"),

    DS_MESSAGE_VIEW(DomainEnum.DS_MESSAGE_DOMAIN_NAME, true, true,
            "DS message view", "Can view DS messages and DS messages detail"),
    DS_MESSAGE_EDIT(DomainEnum.DS_MESSAGE_DOMAIN_NAME, true, false,
            "DS message edit", "Can edit and create DS messages"),
    DS_MESSAGE_DELETE(DomainEnum.DS_MESSAGE_DOMAIN_NAME, true, false,
            "DS message delete", "Can delete DS message"),
    DS_MESSAGE_SHOW_DELETED(DomainEnum.DS_MESSAGE_DOMAIN_NAME, false, false,
            "DS message show deleted", "Can view deleted DS messages"),

    CONTRACT_TYPE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Contract type view", "Can view contract types and contract types detail"),
    CONTRACT_TYPE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Contract type edit", "Can edit and create contract types"),
    CONTRACT_TYPE_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Contract type delete", "Can delete contract type"),

    AREA_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Area view", "Can view areas and areas detail"),
    AREA_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Area edit", "Can edit and create areas"),
    AREA_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Area delete", "Can delete areas"),
    AREA_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Area show deleted", "Can view deleted areas"),

    LABEL_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Label view", "Can view labels and labels detail"),
    LABEL_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Label edit", "Can edit and create labels"),
    LABEL_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Label delete", "Can delete labels"),
    LABEL_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Label show deleted", "Can view deleted labels"),

    LABEL_SUBJECT_VIEW(DomainEnum.SUBJECT_DOMAIN_NAME, false, true,
            "Subject document view", "Can view subject documents and subject documents detail"),
    LABEL_SUBJECT_DELETE(DomainEnum.SUBJECT_DOMAIN_NAME, false, false,
            "Subject label delete", "Can delete subject labels"),
    LABEL_SUBJECT_EDIT(DomainEnum.SUBJECT_DOMAIN_NAME, false, false,
            "Subject label edit", "Can edit and create subject labels"),

    TECHNOLOGY_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Technology view", "Can view technologies and technologies detail"),
    TECHNOLOGY_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Technology edit", "Can edit and create technologies"),
    TECHNOLOGY_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Technology delete", "Can delete technologies"),
    TECHNOLOGY_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Technology show deleted", "Can view deleted technologies"),

    DPH_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "DPH view", "Can view DPHs and DPHs detail"),
    DPH_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "DPH edit", "Can edit and create DPHs"),
    DPH_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "DPH delete", "Can delete DPH"),
    DPH_SHOW_NOT_ALLOWED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "DPH show not allowed", "Can view not allowed DPH"),

    //Enumeration
    ASSET_TYPE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Asset type view", "Can view subject and subject detail"),
    ASSET_TYPE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Asset type edit", "Can edit and create asset types"),
    ASSET_TYPE_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Asset type delete", "Can delete asset types"),
    ASSET_TYPE_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Asset type show deleted", "Can view deleted asset types"),
    ASSET_TYPE_SHOW_NOT_ALLOWED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Asset type show not allowed", "Can view not allowed asset types"),

    OFFER_VIEW(DomainEnum.OFFER_DOMAIN_NAME, true, true,
            "Offer view", "Can view offer and offer detail"),
    OFFER_LIST_VIEW(DomainEnum.OFFER_DOMAIN_NAME, false, false,
            "Offer list view", "Can view offer list"),
    OFFER_EDIT(DomainEnum.OFFER_DOMAIN_NAME, true, false,
            "Offer edit", "Can edit and create offer"),
    OFFER_DELETE(DomainEnum.OFFER_DOMAIN_NAME, true, false,
            "Offer delete", "Can delete offer"),
    OFFER_SHOW_DELETED(DomainEnum.OFFER_DOMAIN_NAME, false, false,
            "Offer show deleted", "Can view deleted offer"),

    OFFER_NOTE_VIEW(DomainEnum.OFFER_DOMAIN_NAME, true, true,
            "Offer note view", "Can view offer notes and opportunity notes detail"),
    OFFER_NOTE_EDIT(DomainEnum.OFFER_DOMAIN_NAME, true, false,
            "Offer notes edit", "Can edit and create offer notes"),
    OFFER_NOTE_DELETE(DomainEnum.OFFER_DOMAIN_NAME, true, false,
            "Offer note delete", "Can delete offer note"),

    OFFER_AREA_TECHNOLOGY_SIGN_EDIT(DomainEnum.OFFER_DOMAIN_NAME, true, false,
            "Offer area technology sign", "Can add sign for area and technology to offer"),
    OFFER_EMAIL_VIEW(DomainEnum.OFFER_DOMAIN_NAME, true, false,
            "Offer email view", "Can view offer emails"),
    OFFER_EMAIL_EDIT(DomainEnum.OFFER_DOMAIN_NAME, true, false,
            "Offer email edit", "Can edit offer emails"),
    OFFER_EMAIL_DELETE(DomainEnum.OFFER_DOMAIN_NAME, true, false,
            "Offer email delete", "Can delete offer emails"),

    //Enumeration
    DOCUMENT_FORMAT_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Document format view", "Can view document formats and document formats detail"),
    DOCUMENT_FORMAT_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Document format edit", "Can edit and create document formats"),
    DOCUMENT_FORMAT_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Document format delete", "Can delete document format"),
    DOCUMENT_FORMAT_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Document formats show deleted", "Can view deleted document formats"),
    DOCUMENT_FORMAT_SHOW_NOT_ALLOWED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Document formats show not allowed", "Can view not allowed document formats"),

    EMPLOYEE_VIEW(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, true,
            "Employee view", "Can view employees and employees detail"),
    EMPLOYEE_EDIT(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, false,
            "Employee edit", "Can edit and create employees"),
    EMPLOYEE_DELETE(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, false,
            "Employee delete", "Can delete employees"),

    EMPLOYEE_DOCUMENT_VIEW(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, true,
            "Employee document view", "Can view employee documents and employee documents detail"),
    EMPLOYEE_DOCUMENT_UPLOAD(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, false,
            "Employee document upload", "Can upload employee documents"),
    EMPLOYEE_DOCUMENT_DELETE(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, false,
            "Employee document delete", "Can delete employee document"),
    EMPLOYEE_DOCUMENT_DOWNLOAD(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, false,
            "Employee document download", "Can download employee documents"),

    //Enumeration
    CONTRACT_STATE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Contract state view", "Can view contract states and contract states detail"),
    CONTRACT_STATE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Contract state edit", "Can edit and create contract states"),
    CONTRACT_STATE_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Contract state delete", "Can delete contract state"),
    CONTRACT_STATE_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Contract state show deleted", "Can view deleted contract state"),
    CONTRACT_STATE_SHOW_NOT_ALLOWED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Contract state show not allowed", "Can view not allowed contract state"),

    USER_SETTINGS_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "User settings view", "Can view activities and user settings detail"),

    TURN_OFF_TRANSLATIONS(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Can turn off translations",
            "Show checkbox in user settings dialog. The checkbox allow turn off translations"),

    TASK_FOLLOWING_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Task following view", "Can view task followings and task followings detail"),
    TASK_FOLLOWING_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Task following edit", "Can edit and create task followings"),
    TASK_FOLLOWING_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Task following delete", "Can delete task following"),

    TASK_TYPE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Task type view", "Can view task type and task type detail"),
    TASK_TYPE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Task type edit", "Can edit and create task type"),
    TASK_TYPE_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Task delete", "Can archive task type"),

    WORK_REPORT_VIEW(DomainEnum.WORK_REPORT_DOMAIN_NAME, false, true,
            "Work report view", "Can view work reports and work reports detail"),

    //Enumeration
    ACTIVITY_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Activity view", "Can view activities and activities detail"),
    ACTIVITY_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Activity edit", "Can edit and create activities"),
    ACTIVITY_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Activity delete", "Can delete activities"),
    ACTIVITY_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Activity show deleted", "Can view deleted activity"),
    ACTIVITY_SHOW_NOT_ALLOWED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Activity show not allowed", "Can view not allowed activity"),

    ACTIVITY_BY_OBJECT_VIEW(DomainEnum.ACTIVITY_DOMAIN_NAME, false, true,
            "Activity by object view", "Can view activities by object and activity by object details"),
    ACTIVITY_BY_OBJECT_LINK(DomainEnum.ACTIVITY_DOMAIN_NAME, false, false,
            "Object activity link", "Can link activity to object"),
    ACTIVITY_BY_OBJECT_UNLINK(DomainEnum.ACTIVITY_DOMAIN_NAME, false, false,
            "Object activity unlink", "Can unlink area from opportunity"),

    EMPLOYEE_BY_OBJECT_VIEW(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, true,
            "Employee by object view", "Can view employees by object and employee by object details"),
    EMPLOYEE_BY_OBJECT_LINK(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, false,
            "Object employee link", "Can link employee to object"),
    EMPLOYEE_BY_OBJECT_UNLINK(DomainEnum.EMPLOYEE_DOMAIN_NAME, false, false,
            "Object employee link", "Can unlink object from employee"),

    PHASE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Phase view", "Can view phases and phases detail"),
    PHASE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Phase edit", "Can edit and create phases"),

    ASSET_NOTE_VIEW(DomainEnum.ASSET_DOMAIN_NAME, true, true,
            "Asset note view", "Can view asset notes and asset notes detail"),
    ASSET_NOTE_EDIT(DomainEnum.ASSET_DOMAIN_NAME, true, false,
            "Asset note edit", "Can edit and create asset notes"),
    ASSET_NOTE_DELETE(DomainEnum.ASSET_DOMAIN_NAME, true, false,
            "Asset note delete", "Can delete asset note"),

    EMAIL_VIEW(DomainEnum.EMAIL_DOMAIN_NAME, false, true,
            "Email view", "Can view emails and emails detail"),
    EMAIL_EDIT(DomainEnum.EMAIL_DOMAIN_NAME, false, false,
            "Email edit", "Can edit and create emails"),
    EMAIL_DELETE(DomainEnum.EMAIL_DOMAIN_NAME, false, false,
            "Email delete", "Can delete emails"),

    CHANGE_OWNER(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Change owner", "Can change owner on entities"),

    APPROVEMENT_APROVER_VIEW(DomainEnum.APPROVEMENT, false, true,
            "Approvement aprover view", "View for approvers"),

    APPROVEMENT_HOME_OFFICE_VIEW(DomainEnum.APPROVEMENT, false, true,
            "Approvement homeoffice view", "View for homeoffices"),

    APPROVEMENT_HOLIDAY_VIEW(DomainEnum.APPROVEMENT, false, true,
            "Approvement holiday view", "View for holiday"),

    APPROVEMENT_UNPAID_LEAVE_VIEW(DomainEnum.APPROVEMENT, false, true,
            "Approvement unpaid leave view", "View for unpaid leave"),

    APPROVEMENT_PAID_LEAVE_VIEW(DomainEnum.APPROVEMENT, false, true,
            "Approvement paid leave view", "View for paid leave"),

    APPROVEMENT_BUSINESS_TRIP_VIEW(DomainEnum.APPROVEMENT, false, true,
            "Approvement business trip view", "View for business trip"),

    APPROVEMENT_ILL_VIEW(DomainEnum.APPROVEMENT, false, true,
            "Approvement ill view", "View for ill"),

    APPROVEMENT_VIEW_ALL(DomainEnum.APPROVEMENT, false, true,
            "Approvement view all", "Can see all approvementy requests"),

    APPROVEMENT_EDIT_ALL(DomainEnum.APPROVEMENT, false, true,
            "Approvement edit all", "Can edit all approvementy requests"),

    VIRTUAL_SERVER_VIEW(DomainEnum.INFRASTRUCTURE, false, true,
            "Virtual server view", "Can view virtual server page"),
    VIRTUAL_SERVER_SHOW_DELETED(DomainEnum.INFRASTRUCTURE, false, false,
            "Virtual server show deleted", "can view deleted virtual servers"),

    //Enumeration
    SUBNET_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Subnet view", "Can view subnet and subnet detail"),
    SUBNET_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Subnet edit", "Can edit and create subnet"),
    SUBNET_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Subnet delete", "Can delete subnet"),
    SUBNET_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Subnet show deleted", "Can view deleted subnet"),
    SUBNET_SHOW_NOT_ALLOWED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Subnet show not allowed", "Can view not allowed subnet"),

    ATTENDANCE_VIEW(DomainEnum.ATTENDANCE, false, true,
            "Attendance view", "View for attendance"),

    ATTENDANCE_DOCUMENT_VIEW(DomainEnum.ATTENDANCE, false, true,
            "Attendance documents view", "View for attendance documents"),

    ATTENDANCE_DOCUMENT_DELETE(DomainEnum.ATTENDANCE, false, true,
            "Attendance documents delete", "Delete attendance documents"),


    EMPLOYEE_CONTRACT_VIEW(DomainEnum.EMPLOYEE_CONTRACT_DOMAIN_NAME, true, true,
            "Employee contract view", "Can view employee contracts"),
    EMPLOYEE_CONTRACT_EDIT(DomainEnum.EMPLOYEE_CONTRACT_DOMAIN_NAME, true, false,
            "Employee contract edit", "Can edit and create employee contract"),
    EMPLOYEE_CONTRACT_DELETE(DomainEnum.EMPLOYEE_CONTRACT_DOMAIN_NAME, false, false,
            "Employee contract delete", "Can delete employee contract"),
    EMPLOYEE_CONTRACT_ARCHIVE(DomainEnum.EMPLOYEE_CONTRACT_DOMAIN_NAME, false, false,
            "Employee contract archive", "Can archive employee contract"),
    EMPLOYEE_CONTRACT_UNARCHIVE(DomainEnum.EMPLOYEE_CONTRACT_DOMAIN_NAME, false, false,
            "Employee contract unarchive", "Can unarchive employee contract"),
    EMPLOYEE_CONTRACT_SHOW_ARCHIVED(DomainEnum.EMPLOYEE_CONTRACT_DOMAIN_NAME, false, false,
            "Employee contract show archived", "Can view archived employee contracts"),

    EMP_CONTRACT_STATE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Employee contract state view", "Can view employee contract state"),
    EMP_CONTRACT_STATE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Employee contract state edit", "Can create and edit employee contract state"),
    EMP_CONTRACT_STATE_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Employee contract state delete", "Can delete employee contract state"),
    EMP_CONTRACT_STATE_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Employee contract state show deleted", "Can view deleted employee contract states"),
    EMP_CONTRACT_STATE_SHOW_NOT_ALLOWED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Employee contract state show not allowed", "Can view not allowed employee contract states"),

    //Enumeration
    PAID_LEAVE_TYPE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Paid leave type view", "Can view paid leave type and paid leave type detail"),
    PAID_LEAVE_TYPE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Paid leave type edit", "Can edit and create paid leave type"),
    PAID_LEAVE_TYPE_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Paid leave type delete", "Can delete paid leave type"),
    PAID_LEAVE_TYPE_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Paid leave type show deleted", "Can view deleted paid leave type"),
    PAID_LEAVE_TYPE_SHOW_NOT_ALLOWED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Paid leave type show not allowed", "Can view not allowed paid leave type"),

    BUSINESS_TRIP_PURPOSE_VIEW(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, true,
            "Business trip purpose view", "Can view Business trip purpose and subnet detail"),
    BUSINESS_TRIP_PURPOSE_EDIT(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Business trip purpose edit", "Can edit and create Business trip purpose"),
    BUSINESS_TRIP_PURPOSE_DELETE(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Business trip purpose delete", "Can delete Business trip purpose"),
    BUSINESS_TRIP_PURPOSE_SHOW_DELETED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Business trip purpose show deleted", "Can view deleted Business trip purpose"),
    BUSINESS_TRIP_PURPOSE_SHOW_NOT_ALLOWED(DomainEnum.ADMINISTRATION_DOMAIN_NAME, false, false,
            "Business trip purpose show not allowed", "Can view not allowed Business trip purpose"),

    TRANSLATION_VIEW(DomainEnum.INFRASTRUCTURE, false, true,
            "Translation view", "Can view translation UI");

    private final DomainEnum domainEnum;
    private final boolean isCustomPermission;
    private final boolean canView;
    private final String permissionTitle;
    private final String permissionDesc;

    Permission(DomainEnum domainEnum, boolean isCustomPermission, boolean canView, String permissionTitle,
               String permissionDesc) {
        this.domainEnum = domainEnum;
        this.isCustomPermission = isCustomPermission;
        this.canView = canView;
        this.permissionTitle = permissionTitle;
        this.permissionDesc = permissionDesc;
    }

    public static Set<String> getCustomStringSetByObjectName(String key) {
        Set<String> permStrList = new LinkedHashSet<>();
        for (Permission permission : Permission.values()) {
            if (permission.getObjectName().equals(key) && permission.isCustomPermission) {
                permStrList.add(permission.name());
            }
        }
        return permStrList;
    }

    public static Set<Permission> getCustomByObjectName(String key) {
        Set<Permission> permStrList = new LinkedHashSet<>();
        for (Permission permission : Permission.values()) {
            if (permission.getObjectName().equals(key) && permission.isCustomPermission) {
                permStrList.add(permission);
            }
        }
        return permStrList;
    }

    public static Map<DomainEnum, List<Permission>> getPermissionMap() {
        Map<DomainEnum, List<Permission>> permissionMap = new HashMap<>();
        for (Permission permission : Permission.values()) {
            if (DomainEnum.EMPTY_DOMAIN != permission.domainEnum) {
                List<Permission> permissionList;
                if (permissionMap.containsKey(permission.domainEnum)) {
                    permissionList = permissionMap.get(permission.domainEnum);
                } else {
                    permissionList = new ArrayList<>();
                }
                permissionList.add(permission);
                permissionMap.put(permission.domainEnum, permissionList);
            }
        }
        return permissionMap;
    }

    public static Map<DomainEnum, List<Permission>> getCustomPermissionMap() {
        Map<DomainEnum, List<Permission>> customPermissionMap = new HashMap<>();
        for (Permission permission : Permission.values()) {
            if (permission.isCustomPermission) {
                List<Permission> permissionList;
                if (customPermissionMap.containsKey(permission.domainEnum)) {
                    permissionList = customPermissionMap.get(permission.domainEnum);
                } else {
                    permissionList = new ArrayList<>();
                }
                permissionList.add(permission);
                customPermissionMap.put(permission.domainEnum, permissionList);
            }
        }
        return customPermissionMap;
    }

    public static Map<String, Set<String>> getReadCustomPermissionMap() {
        Map<String, Set<String>> customPermissionMap = new HashMap<>();
        for (Permission permission : Permission.values()) {
            if (permission.isCustomPermission && permission.canView) {
                Set<String> permissionSet;
                if (customPermissionMap.containsKey(permission.domainEnum.getValue())) {
                    permissionSet = customPermissionMap.get(permission.domainEnum.getValue());
                } else {
                    permissionSet = new HashSet<>();
                }
                permissionSet.add(permission.name());
                customPermissionMap.put(permission.domainEnum.getValue(), permissionSet);
            }
        }
        return customPermissionMap;
    }

    public static Set<String> getNoteReadPermissionSet() {
        Set<String> permissionSet = new HashSet<>();
        permissionSet.add(ASSET_NOTE_VIEW.name());
        permissionSet.add(OPPORTUNITY_NOTE_VIEW.name());
        permissionSet.add(CONTRACT_NOTE_VIEW.name());
        permissionSet.add(PROJECT_NOTE_VIEW.name());
        permissionSet.add(SUBJECT_NOTE_VIEW.name());
        permissionSet.add(CONTACT_PERSON_NOTE_VIEW.name());
        return permissionSet;
    }

    public static Map<String, Set<String>> getCustomPermissionStringMap() {
        Map<String, Set<String>> domainPermMap = new HashMap<>();
        for (Permission permission : Permission.values()) {
            if (NON_EXISTENT_PERMISSION != permission && permission.isCustomPermission) {
                Set<String> permSet;
                if (domainPermMap.containsKey(permission.getObjectName())) {
                    permSet = domainPermMap.get(permission.getObjectName());
                } else {
                    permSet = new HashSet<>();
                }
                permSet.add(permission.name());
                domainPermMap.put(permission.getObjectName(), permSet);
            }
        }
        return domainPermMap;
    }

    @Override
    public String getAuthority() {
        return this.name();
    }

    public static Permission valueOfOrNotExists(String value) {
        for (Permission permission : Permission.values()) {
            if (permission.name().equals(value)) {
                return permission;
            }
        }
        if (!CustomPermissionService.ALL_PERMISSION.equals(value)) {
            log.error("Permission does not exists: " + value);
        }
        return NON_EXISTENT_PERMISSION;
    }

    public String getObjectName() {
        return domainEnum.getValue();
    }

    public boolean canView() {
        return canView;
    }

    public String getTitle() {
        return permissionTitle;
    }

    public String getDescription() {
        return permissionDesc;
    }
}
