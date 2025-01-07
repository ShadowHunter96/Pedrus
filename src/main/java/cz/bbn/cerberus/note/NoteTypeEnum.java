package cz.bbn.cerberus.note;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.permission.Permission;

public enum NoteTypeEnum {
    CONTACT_PERSON(DomainEnum.CONTACT_PERSON_DOMAIN_NAME.getValue(),
            Permission.CONTACT_PERSON_NOTE_VIEW, Permission.CONTACT_PERSON_NOTE_EDIT,
            Permission.CONTACT_PERSON_NOTE_DELETE),
    PROJECT(DomainEnum.PROJECT_DOMAIN_NAME.getValue(), Permission.PROJECT_NOTE_VIEW, Permission.PROJECT_NOTE_EDIT,
            Permission.PROJECT_NOTE_DELETE),
    SUBJECT(DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), Permission.SUBJECT_NOTE_VIEW, Permission.SUBJECT_NOTE_EDIT,
            Permission.SUBJECT_NOTE_DELETE),
    CONTRACT(DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), Permission.CONTRACT_NOTE_VIEW, Permission.CONTRACT_NOTE_EDIT,
            Permission.CONTRACT_NOTE_DELETE),
    OPPORTUNITY(DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), Permission.OPPORTUNITY_NOTE_VIEW,
            Permission.OPPORTUNITY_EDIT, Permission.OPPORTUNITY_NOTE_DELETE),
    ASSET(DomainEnum.ASSET_DOMAIN_NAME.getValue(), Permission.ASSET_NOTE_VIEW, Permission.ASSET_NOTE_EDIT,
            Permission.ASSET_NOTE_DELETE),
    OFFER(DomainEnum.OFFER_DOMAIN_NAME.getValue(), Permission.OFFER_NOTE_VIEW, Permission.OFFER_NOTE_EDIT,
            Permission.OFFER_NOTE_DELETE),
    ANY(TextValues.SHOW_ALL_TEXT_VALUE, Permission.NON_EXISTENT_PERMISSION, Permission.NON_EXISTENT_PERMISSION,
            Permission.NON_EXISTENT_PERMISSION);

    private final String objectName;
    private final Permission viewPermission;
    private final Permission editPermission;
    private final Permission deletePermission;

    NoteTypeEnum(String objectName, Permission viewPermission, Permission editPermission, Permission deletePermission) {
        this.objectName = objectName;
        this.viewPermission = viewPermission;
        this.editPermission = editPermission;
        this.deletePermission = deletePermission;
    }

    public String getObjectName() {
        return objectName;
    }

    public static NoteTypeEnum convertFromObjectName(String objectName) {
        for (NoteTypeEnum noteTypeEnum : NoteTypeEnum.values()) {
            if (noteTypeEnum.objectName.equals(objectName)) {
                return noteTypeEnum;
            }
        }
        return ANY;
    }

    public static NoteTypeEnum getFromString(String noteTypeEnumStr) {
        for (NoteTypeEnum noteTypeEnum : NoteTypeEnum.values()) {
            if (noteTypeEnum.name().equals(noteTypeEnumStr)) {
                return noteTypeEnum;
            }
        }
        return ANY;
    }

    public Permission getViewPerm() {
        return viewPermission;
    }

    public Permission getEditPerm() {
        return editPermission;
    }

    public Permission getDeletePerm() {
        return deletePermission;
    }
}
