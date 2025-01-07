package cz.bbn.cerberus.contactperson.ui.components;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.button.Button;

public interface ContactPersonGetMultipleEvent {

    ComponentEventListener<ClickEvent<Button>> get(ContactPersonLinkDialog dialog, Button button);
}
