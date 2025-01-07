package cz.bbn.cerberus.document.interfaces;

public interface DeleteOrUnlinkAction {

    void deleteItem(String id, String entityId, boolean unlink);
}
