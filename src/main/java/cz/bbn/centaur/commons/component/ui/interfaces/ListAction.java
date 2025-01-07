package cz.bbn.cerberus.commons.component.ui.interfaces;

import java.util.List;

public interface ListAction<T> {
    List<T> getList(String id);
}
