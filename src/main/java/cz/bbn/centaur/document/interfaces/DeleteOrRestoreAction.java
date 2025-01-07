package cz.bbn.cerberus.document.interfaces;

public interface DeleteOrRestoreAction {

    void deleteOrRestoreItem(String name, boolean restore);
}
