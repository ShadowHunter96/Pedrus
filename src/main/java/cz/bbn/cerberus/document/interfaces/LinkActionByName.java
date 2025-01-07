package cz.bbn.cerberus.document.interfaces;

import cz.bbn.cerberus.document.DocumentObjectEnum;

public interface LinkActionByName {

    void linkAction(String name, String oldDocumentId, DocumentObjectEnum documentObjectEnum, String entityId);
}
