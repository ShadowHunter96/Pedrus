package cz.bbn.cerberus.commons.component.ui.interfaces;

public interface SaveAction<T> {
    void saveItem(T dto, T originalDto);
}
