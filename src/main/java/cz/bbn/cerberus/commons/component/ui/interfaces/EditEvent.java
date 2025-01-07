package cz.bbn.cerberus.commons.component.ui.interfaces;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.button.Button;

public interface EditEvent {

    ComponentEventListener<ClickEvent<Button>> event(String id);
}
