package cz.bbn.cerberus.commons.component.ui.interfaces;

import com.vaadin.flow.data.binder.Binder;

public interface AppBinderOperation<T> {

    Binder<T> getBinder();

    T getDto();
}
