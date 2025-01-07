package cz.bbn.cerberus.document.interfaces;

public interface DialogSaveAction<T> {
    boolean save(T dto, T originalDto);
}
